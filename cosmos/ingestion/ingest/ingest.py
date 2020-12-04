"""
ingest.py

Main file for handling ingestion related activities
"""
import pickle
import shutil
import functools
import json
import os
from PIL import Image
import io
import subprocess
import glob
from ingest.process_page import propose_and_pad, xgboost_postprocess, rules_postprocess
from ingest.detect import detect
from dask.distributed import Client, progress, as_completed
from ingest.utils.preprocess import resize_png
from ingest.utils.pdf_helpers import get_pdf_names
from ingest.utils.pdf_extractor import parse_pdf
from ingest.process.ocr.ocr import regroup, pool_text
from ingest.process.aggregation.aggregate import aggregate_router
from ingest.process.representation_learning.compute_word_vecs import make_vecs
import pandas as pd
import signal
import logging
from itertools import islice
from typing import List

logging.basicConfig(format='%(levelname)s :: %(filename) :: %(funcName)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logging.getLogger("asyncio").setLevel(logging.ERROR)
logging.getLogger("pdfminer").setLevel(logging.ERROR)
logging.getLogger("PIL").setLevel(logging.ERROR)
logging.getLogger("ingest.detect").setLevel(logging.ERROR)
logging.getLogger("ingest.process.detection.src.torch_model.model.model").setLevel(logging.ERROR)
logging.getLogger("ingest.process.detection.src.utils.ingest_images").setLevel(logging.ERROR)
logging.getLogger("ingest.process.detection.src.torch_model.train.data_layer.xml_loader").setLevel(logging.ERROR)

logger = logging.getLogger(__name__)
logger.setLevel(logging.ERROR)


class Ingest:
    """
    Ingest class
    Handles running the ingestion pipeline
    """
    def __init__(self, scheduler_address, use_semantic_detection=False, client=None,
                       tmp_dir=None, use_xgboost_postprocess=False, use_rules_postprocess=False):
        """
        :param scheduler_address: Address to existing Dask scheduler
        :param use_semantic_detection: Whether or not to run semantic detection
        :param client: A Dask client. Can be passed in. If None, one will be created to connect to the scheduler
        :param tmp_dir: Path to temporary directory which intermediate files and images will be written
        :param use_xgboost_postprocess: Whether to use the XGBoost postprocessing model
        :param use_rules_postprocess: Whether to utilize the rules postprocessing, which is specific to scientific docs
        """
        logger.info("Initializing Ingest object")
        self.client = client
        if self.client is None:
            logger.info("Setting up client")
            self.client = Client(scheduler_address, serializers=['msgpack', 'dask'], deserializers=['msgpack', 'dask', 'pickle'])
            logger.info(self.client)
        self.use_xgboost_postprocess = use_xgboost_postprocess
        self.use_rules_postprocess = use_rules_postprocess
        self.use_semantic_detection = use_semantic_detection
        self.tmp_dir = tmp_dir
        if tmp_dir is None:
            raise ValueError("tmp_dir must be passed in")
        # Create a subdirectory for tmp files
        self.images_tmp = os.path.join(self.tmp_dir, 'images')
        os.makedirs(self.images_tmp, exist_ok=True)

    def __del__(self):
        """Simple client cleanup"""
        if self.client is not None:
            self.client.close()

    def ingest(self,
               pdf_directory,
               dataset_id,
               result_path,
               images_pth,
               skip_ocr=True,
               visualize_proposals=False,
               aggregations=[],
               batch_size=2000,
               compute_word_vecs=False,
               ngram=1,
               enrich=False,
               threshold=0.8,
               spans=20):
        """
        Handler for ingestion pipeline.

        Given a directory of PDFs, run the cosmos ingestion pipeline. This will identifies page objects, and optionally
        perform aggregations over objects (eg associating tables with table captions in scientific document pipelines)

        By default, a single parquet file will be written, containing each identified page object and its text.

        If additional aggregations are defined, a parquet file will be written for each defined aggregation.

        For additional information on the aggregations and schemas for the output files, see the documentation.

        :param pdf_directory: path to a directory of PDFs to process
        :param dataset_id: The dataset id for this PDF set
        :param result_path: Path to output directory where parquets and additional images will be written
        :param images_pth: Path to where images can be written to (tmp, not output images directory)
        :param skip_ocr: If True, PDFs with no metadata associated will be skipped. If False, OCR will be performed
        :param visualize_proposals: Debugging option, will write images with bounding boxes from proposals to tmp
        :param aggregations: List of aggregations to run over resulting objects
        :param compute_word_vecs: Whether to compute word vectors over the corpus
        :param ngram: n in ngram for word vecs
        :param enrich: If true run semantic enrichment on ingest output parquets
        :param threshold: postprocess_score threshold for identifying an object for context enrichment
        :param spans: number of words either side of an object coreference to capture for context
        """
        os.makedirs(images_pth, exist_ok=True)
        pdfnames = get_pdf_names(pdf_directory)
        pdf_to_images = functools.partial(Ingest.pdf_to_images, dataset_id, self.images_tmp)
        logger.info('Starting ingestion. Converting PDFs to images.')
        images = [self.client.submit(pdf_to_images, pdf, resources={'process': 1}) for pdf in pdfnames]
        class TimeOutError(Exception):
            pass
        def raise_timeout(var1, var2):
            raise TimeOutError
        signal.signal(signal.SIGALRM, raise_timeout)

        try:
            for _ in as_completed(images):
                signal.alarm(0)
                signal.alarm(180)
        except TimeOutError:
            images = [i for i in images if i.status == 'finished']
            pass
        else:
            signal.alarm(0)
        logger.info('Done converting to images. Starting detection and text extraction')
        images = [i.result() for i in images]
        images = [i for i in images if i is not None]
        images_queue = [i for il in images for i in il]
        images = []
        iterator = iter(images_queue)
        while chunk := list(islice(iterator, batch_size)):
            partial_propose = functools.partial(propose_and_pad, visualize=visualize_proposals)
            chunk = self.client.map(partial_propose, chunk, resources={'process': 1}, priority=8)
            if self.use_semantic_detection:
                chunk = self.client.map(detect, chunk, resources={'GPU': 1}, priority=8)
                chunk = self.client.map(regroup, chunk, resources={'process': 1})
                pool_text_ocr_opt = functools.partial(pool_text, skip_ocr=skip_ocr)
                chunk = self.client.map(pool_text_ocr_opt, chunk, resources={'process': 1})
                if self.use_xgboost_postprocess:
                    chunk = self.client.map(xgboost_postprocess, chunk, resources={'process': 1})
                    chunk = [i for i in chunk if i.result() != '']
                    if self.use_rules_postprocess:
                        chunk = self.client.map(rules_postprocess, chunk, resources={'process': 1})
            progress(chunk)
            images.extend([i.result() for i in chunk])
        results = []
        for i in images:
            with open(i, 'rb') as rf:
                obj = pickle.load(rf)
                for ind, c in enumerate(obj['content']):
                    bb, cls, text = c
                    scores, classes = zip(*cls)
                    scores = list(scores)
                    classes = list(classes)
                    postprocess_cls = postprocess_score = None
                    if 'xgboost_content' in obj:
                        _, postprocess_cls, _, postprocess_score = obj['xgboost_content'][ind]
                    final_obj = {'pdf_name': obj['pdf_name'],
                                 'dataset_id': obj['dataset_id'],
                                 'page_num': obj['page_num'],
                                 'img_pth': obj['pad_img'],
                                 'pdf_dims': list(obj['pdf_limit']),
                                 'bounding_box': list(bb),
                                 'classes': classes,
                                 'scores': scores,
                                 'content': text,
                                 'postprocess_cls': postprocess_cls,
                                 'postprocess_score': postprocess_score
                                }
                    results.append(final_obj)
        if len(results) == 0:
            logger.info('No objects found')
            return
        result_df = pd.DataFrame(results)
        result_df['detect_cls'] = result_df['classes'].apply(lambda x: x[0])
        result_df['detect_score'] = result_df['scores'].apply(lambda x: x[0])
        for aggregation in aggregations:
            aggregate_df = aggregate_router(result_df, aggregate_type=aggregation, write_images_pth=images_pth)
            name = f'{dataset_id}_{aggregation}.parquet'
            aggregate_df.to_parquet(os.path.join(result_path, name), engine='pyarrow', compression='gzip')
        if compute_word_vecs:
            make_vecs(result_df, ngram)
        result_df.to_parquet(os.path.join(result_path, f'{dataset_id}.parquet'), engine='pyarrow', compression='gzip')

        if enrich:
            logger.info('start enrich process')
            self.enrich(file_path=result_path,
                        dataset_id=dataset_id,
                        threshold=threshold,
                        spans=spans)

    @staticmethod
    def needed_columns_are_in_df(to_check: List, to_interrogate: List) -> bool:
        """
        True if all of to_check values are in to_interrogate, else False
        """
        if all(x in to_interrogate for x in to_check):
            return True
        else:
            return False

    def enrich(self, file_path: str, dataset_id: str, threshold: float, spans: int):
        """
        iterate over all ingest output parquets and run distributed context enrichment process
        :param file_path: a directory full of parquets (ingest output) to process
        :param dataset_id: ingest process dataset_id
        :param threshold: float cut off for postprocess table detection score to process as table caption
        :param spans: number of words each side of label to pull in as context for each table label in content text
                if None will use regex to pull out full stop to full stop span around the table label
        """

        for pq in glob.glob(os.path.join(file_path, '*.parquet')):
            logger.info(f'processing file: {pq}')
            df = pd.read_parquet(pq)
            basename = os.path.basename(pq)

            needed_columns = [
                'content',
                'postprocess_cls',
                'postprocess_score'
            ]

            if Ingest.needed_columns_are_in_df(needed_columns, list(df.columns)):

                if dataset_id:
                    logger.info(f'limit enrichment to dataset id: {dataset_id}')
                    df = df[df['dataset_id'] == dataset_id]

                # GET ALL DOCUMENTS, LIST OF DFs
                all_pdf_names = list(df.pdf_name.unique())
                single_doc_dfs = []

                logger.info('split ingest output into docs')
                for name in all_pdf_names:
                    single_doc_dfs.append(df[df['pdf_name'] == name])

                partial_get_context = functools.partial(Ingest.get_contexts,
                                                        threshold,
                                                        spans)
                logger.info(f'start enrichment processing with doc count {len(single_doc_dfs)}')
                enriched = [self.client.submit(partial_get_context,
                                               doc_df,
                                               resources={'process': 1})
                            for doc_df in single_doc_dfs]
                progress(enriched)
                logger.info('collecting all enriched docs')
                enriched = [e.result() for e in enriched]
                df = pd.concat(enriched)
                logger.info(f'size of df returned from enrichment: {len(df)}')
                df = df.reset_index(drop=True)

            else:
                pass
            logger.info(f'outputting data: {os.path.join(file_path, basename)}')
            df.to_parquet(os.path.join(file_path, basename))

    @classmethod
    def get_contexts(cls, threshold, spans, doc_df):
        """
        perform context enrichment per doc in ingest output parquet - code to run on dask cluster worker
        :param threshold: postprocess_score value needed to act on a given postprocess_cls Table or Table Caption
        :param spans: number of words either side of a label to capture as context in doc content
        :param doc_df: input dataframe - one doc of output from ingest pipeline
        :return doc_df: input dataframe with any enriched rows added
        """
        # disable SettingWithCopyWarning - safe since always over writing original
        pd.options.mode.chained_assignment = None
        original_df = doc_df.copy()

        label_length = 2
        classes = ['Table', 'Table Caption']

        # get first label_length words of every content cell in df if it's classified correctly and scored highly enough
        logger.info('setting table labels')
        doc_df['table_label'] = [
            ' '.join(content.split()[:label_length])
            if (pp_cls in classes) and (pp_score >= threshold)
            else None
            for content, pp_cls, pp_score in zip(doc_df['content'],
                                                 doc_df['postprocess_cls'],
                                                 doc_df['postprocess_score'])
        ]

        # if table_label series has no values, just return original df
        if len(doc_df[doc_df["table_label"].astype(bool)]) == 0:
            return original_df

        # aggregate all doc words into one list of words
        words = ' '.join(doc_df.content.tolist()).split()

        # mark where each label occurs in list of all doc words
        doc_df['indices'] = [
            [
                j
                for j in range(len(words))
                if table_label == ' '.join(words[j:j + label_length])
            ]
            for table_label in doc_df['table_label']
        ]

        # iterate over list in indices column to extract words before labels in content
        logger.info('getting semantic context')
        doc_df['prefix'] = [
            [
                ' '.join(words[i - spans:i])
                if i - spans >= 0
                else ' '.join(words[0:i])
                for i in index
            ]
            for index in doc_df['indices']
        ]

        # iterate over list in indices column to extract words after labels in content
        doc_df['suffix'] = [
            [
                ' '.join(words[i + label_length:i + label_length + spans])
                if i + label_length + spans <= len(words)
                else ' '.join(words[i + label_length:len(words)])
                for i in index
            ]
            for index in doc_df['indices']
        ]

        # join prefixes and suffixes together
        doc_df['context'] = [
            [
                prefix[i] + ' ' + suffix[i]
                for i in range(len(prefix))
            ]
            for prefix, suffix in zip(doc_df['prefix'], doc_df['suffix'])
        ]

        # remove rows without context (empty lists)
        logger.info('removing rows with no semantic context')
        doc_df = doc_df[doc_df['context'].apply(lambda x: len(x) > 0)]

        # replace content with context as text only
        logger.info('replace content with context')
        doc_df['content'] = [' '.join(x) for x in doc_df['context']]

        # append context only df to original df using only original columns
        logger.info('append context to original and return')
        return original_df.append(doc_df.loc[:, list(original_df.columns)])

    def write_images_for_annotation(self, pdf_dir, img_dir):
        """
        Helper function that will write images from PDFs for annotation.
        :param pdf_dir: Path to PDFs to write images for
        :param img_dir: Output directory where images will be written
        """
        logger.info(f"Converting PDFs to images and writing to target directory: {img_dir}")
        pdfnames = get_pdf_names(pdf_dir)
        pdf_to_images = functools.partial(Ingest.pdf_to_images, 'na', self.images_tmp)
        images = [self.client.submit(pdf_to_images, pdf, resources={'process': 1}) for pdf in pdfnames]
        progress(images)
        images = [i.result() for i in images]
        images = [i for i in images if i is not None]
        images = [i for il in images for i in il]
        paths = [f'{tmp_dir}/{pdf_name}_{pn}' for tmp_dir, pdf_name, pn in images]
        for path in paths:
            bname = os.path.basename(path)
            new_bname = bname + '.png'
            shutil.copy(path, os.path.join(img_dir, new_bname))
        logger.info('Done.')
        shutil.rmtree(self.tmp_dir)

    @classmethod
    def pdf_to_images(cls, dataset_id, tmp_dir, filename):
        """
        Convert PDFs to images, and log image-pdf provenance. Writes pickle files that will be handled later.
        :param dataset_id: Dataset id for this PDF set
        :param tmp_dir: tmp directory where images and pickle files will be written
        :param filename: Path to PDF file
        :return: [(tmp_dir, pdf_name, page_num)], list of each pdf and the pages associated with it
        """
        if filename is None:
            return None
        pdf_name = os.path.basename(filename)
        limit = None

        try:
            meta, limit = parse_pdf(filename)
            logger.debug(f'Limit: {limit}')
        except TypeError as te:
            logger.error(str(te), exc_info=True)
            logger.error(f'Logging TypeError for pdf: {pdf_name}')
            return []
        except Exception as e:
            logger.warning(str(e), exc_info=True)
            logger.warning(f'Logging parsing error for pdf: {pdf_name}')
            return []

        subprocess.run(['gs', '-dBATCH',
                        '-dNOPAUSE',
                        '-sDEVICE=png16m',
                        '-dGraphicsAlphaBits=4',
                        '-dTextAlphaBits=4',
                        '-r600',
                        f'-sOutputFile="{tmp_dir}/{pdf_name}_%d"',
                        filename
                        ], stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
        objs = []
        names = glob.glob(f'{tmp_dir}/{pdf_name}_?')
        for image in names:
            try:
                page_num = int(image[-1])
            except ValueError:
                raise Exception(f'{image}')
            with open(image, 'rb') as bimage:
                bstring = bimage.read()
            bytesio = io.BytesIO(bstring)
            try:
                img = Image.open(bytesio).convert('RGB')
            except Exception as e:
                logger.error(str(e), exc_info=True)
                logger.error(f'Image opening error pdf: {pdf_name}')
                return []

            orig_w, orig_h = img.size
            meta2 = None
            img, img_size = resize_png(img, return_size=True)
            w, h = img_size
            dims = [0, 0, w, h]
            if meta is not None:
                orig_w = limit[2]
                orig_h = limit[3]
                scale_w = w / orig_w
                scale_h = h / orig_h
                logger.debug(f'Original w: {orig_w}')
                logger.debug(f'Original h: {orig_h}')
                logger.debug(f'New w: {w}')
                logger.debug(f'New h: {h}')
                meta2 = meta.copy()
                meta2.loc[meta2.page == (page_num-1), 'x1'] = meta2.x1 * scale_w
                meta2.loc[meta2.page == (page_num-1), 'x2'] = meta2.x2 * scale_w
                meta2.loc[meta2.page == (page_num-1), 'y1'] = meta2.y1 * scale_h
                meta2.loc[meta2.page == (page_num-1), 'y2'] = meta2.y2 * scale_h
                meta2 = meta2.to_dict()
                meta2 = json.dumps(meta2)
                meta2 = json.loads(meta2)

            # Convert it back to bytes
            img.save(image, format='PNG')
            obj = {'orig_w': orig_w, 'orig_h': orig_h, 'dataset_id': dataset_id, 'pdf_name': pdf_name, 'meta': meta2, 'dims': dims, 'pdf_limit': limit, 'page_num': page_num}
            if tmp_dir is not None:
                with open(os.path.join(tmp_dir, pdf_name) + f'_{page_num}.pkl', 'wb') as wf:
                    pickle.dump(obj, wf)
                objs.append((tmp_dir, pdf_name, page_num))
            else:
                objs.append(obj)
        return objs

