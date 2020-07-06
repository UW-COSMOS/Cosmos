from ingest.schema import Pdf, Page, PageObject
import pickle
import uuid
import tempfile
import shutil
from dask_cuda import LocalCUDACluster
from ingest.preprocess import resize_png
import functools
import json
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import os
import io
import subprocess
import glob
from PIL import Image
import base64
from ingest.process_page import process_page as pp, postprocess_page, propose_and_pad, xgboost_postprocess, rules_postprocess
from ingest.process_setup import ProcessPlugin
from ingest.detect import detect
from ingest.detect_setup import DetectPlugin
from dask.distributed import get_client, fire_and_forget, Client, LocalCluster, progress
from ingest.utils.pdf_helpers import get_pdf_names
from ingest.pdf_extractor import parse_pdf
from ingest.process.ocr.ocr import regroup, pool_text
import pikepdf
import pandas as pd

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logging.getLogger("asyncio").setLevel(logging.ERROR)
logging.getLogger("pdfminer").setLevel(logging.ERROR)
logging.getLogger("PIL").setLevel(logging.ERROR)
logging.getLogger("ingest.detect").setLevel(logging.ERROR)
logging.getLogger("ingest.process.detection.src.torch_model.model.model").setLevel(logging.ERROR)
logging.getLogger("ingest.process.detection.src.utils.ingest_images").setLevel(logging.ERROR)
logging.getLogger("ingest.process.detection.src.torch_model.train.data_layer.xml_loader").setLevel(logging.ERROR)

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Ingest:
    def __init__(self, scheduler_address, use_semantic_detection=False, client=None,
                       tmp_dir=None, use_xgboost_postprocess=False, use_rules_postprocess=False, xgboost_config=None):
        logger.info("Initializing Ingest object")
        self.client = client
        if self.client is None:
            logger.info("Setting up client")
            self.client = Client(scheduler_address, serializers=['msgpack', 'dask'], deserializers=['msgpack', 'dask'])
            logger.info(self.client)
        self.use_xgboost_postprocess = use_xgboost_postprocess
        self.use_rules_postprocess = use_rules_postprocess
        self.use_semantic_detection = use_semantic_detection
        if use_semantic_detection:
            if self.use_xgboost_postprocess:
                if xgboost_config is None:
                    raise ValueError("If using xgboost postprocess, you need to specify xgboost_config")
                plugin = ProcessPlugin(**xgboost_config)
                self.client.register_worker_plugin(plugin)
        self.tmp_dir = tmp_dir
        if self.tmp_dir is not None:
            # Create a subdirectory for tmp files
            self.images_tmp = os.path.join(self.tmp_dir, 'images')
            os.makedirs(self.images_tmp, exist_ok=True)

    def __del__(self):
        if self.client is not None:
            self.client.close()

    def ingest(self, pdf_directory, dataset_id, result_path, remove_watermark=False):
        if self.tmp_dir is not None:
            self._ingest_local(pdf_directory, dataset_id, result_path, remove_watermark=remove_watermark)
        else:
            self._ingest_distributed(pdf_directory, dataset_id)

    def _ingest_local(self, pdf_directory, dataset_id, result_path, remove_watermark=False):
        pdfnames = get_pdf_names(pdf_directory)
        pdf_to_images = functools.partial(Ingest.pdf_to_images, dataset_id, self.images_tmp)
        if remove_watermark:
            logger.info('Removing watermarks')
            pdfs = []
            for pdf in pdfnames:
                pdfs.append(Ingest.remove_watermark(pdf))
            #pdfs = [self.client.submit(Ingest.remove_watermark, pdf, resources={'process': 1}) for pdf in pdfnames]
            images = self.client.map(pdf_to_images, pdfs)
        else:
            logger.info('Starting ingestion. Converting PDFs to images.')
            images = [self.client.submit(pdf_to_images, pdf, resources={'process': 1}) for pdf in pdfnames]
        progress(images)
        logger.info('Done converting to images. Starting detection and text extraction')
        images = [i.result() for i in images]
        images = [i for i in images if i is not None]
        images = [i for il in images for i in il]
        images = self.client.map(propose_and_pad, images, resources={'process': 1}, priority=8)
        if self.use_semantic_detection:
            images = self.client.map(detect, images, resources={'GPU': 1}, priority=8)
            images = self.client.map(regroup, images, resources={'process': 1})
            images = self.client.map(pool_text, images, resources={'process': 1})
            if self.use_xgboost_postprocess:
                images = self.client.map(xgboost_postprocess, images, resources={'process': 1})
                if self.use_rules_postprocess:
                    images = self.client.map(rules_postprocess, images, resources={'process': 1})
        progress(images)
        images = [i.result() for i in images]
        results = []
        for i in images:
            with open(i, 'rb') as rf:
                obj = pickle.load(rf)
                for c in obj['content']:
                    bb, cls, text = c
                    scores, classes = zip(*cls)
                    scores = list(scores)
                    classes = list(classes)
                    #cls = [list(c) for c in cls]
                    final_obj = {'pdf_name': obj['pdf_name'], 
                                 'dataset_id': obj['dataset_id'],
                                 'page_num': obj['page_num'], 
                                 'bounding_box': list(bb),
                                 'classes': classes,
                                 'scores': scores,
                                 'content': text
                                }
                    results.append(final_obj)
        result_df = pd.DataFrame(results)
        result_df.to_parquet(result_path, engine='pyarrow', compression='gzip')
        shutil.rmtree(self.tmp_dir)

    def _ingest_distributed(self, pdf_directory, dataset_id):
        raise NotImplementedError("Distributed setup currently not implemented via Ingest class. Set a tmp directory.")

    @classmethod
    def remove_watermark(cls, filename):
        target = pikepdf.Pdf.open(filename)
        new = pikepdf.Pdf.new()
        try:
            for ind, page in enumerate(target.pages):
                commands = []
                BDC = False
                for operands, operator in pikepdf.parse_content_stream(page):
                    #for o in operands:
                    #    if type(o) == pikepdf.Dictionary:
                    #        print(o)
                    #        if '/Subtype' in o:
                    #            print(o['/Subtype'])
                    #if str(operator) == 'BDC':
                    #    BDC = True
                    #    continue
                    #if BDC:
                    #    if str(operator) == 'EMC':
                    #        BDC = False
                    #        continue
                    #    continue
                    commands.append((operands, operator))
                new_content_stream = pikepdf.unparse_content_stream(commands)
                new.add_blank_page()
                new.pages[ind].Contents = new.make_stream(new_content_stream)
                new.pages[ind].Resources = new.copy_foreign(target.make_indirect(target.pages[ind].Resources))
            new.remove_unreferenced_resources()
            new.save(filename)
            return filename
        except RuntimeError as e:
            os.remove(filename)
            logger.error(f'Error in file: {filename}')
            logger.error(e)
            return None

    @classmethod
    def pdf_to_images(cls, dataset_id, tmp_dir, filename):
        if filename is None:
            return None
        pdf_name = os.path.basename(filename)
        try:
            meta, dims = parse_pdf(filename)
        except Exception as e:
            logger.error(str(e), exc_info=True)
            raise Exception('Parsing error', str(e))
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
            img, img_size = resize_png(bytesio, return_size=True)
            w, h = img_size
            if meta is not None:
                dims = list(dims)
                orig_w, orig_h = dims[2], dims[3]
                scale_w = w / orig_w
                scale_h = h / orig_h
                meta.loc[meta.page == (page_num-1), 'x1'] = meta.x1 * scale_w
                meta.loc[meta.page == (page_num-1), 'x2'] = meta.x2 * scale_w
                meta.loc[meta.page == (page_num-1), 'y1'] = meta.y1 * scale_h
                meta.loc[meta.page == (page_num-1), 'y2'] = meta.y2 * scale_h
                dims = [0, 0, w, h]
                meta2 = meta.to_dict()
                meta2 = json.dumps(meta2)
                meta2 = json.loads(meta2)

            # Convert it back to bytes
            img.save(image, format='PNG')
            obj = {'dataset_id': dataset_id, 'pdf_name': pdf_name, 'meta': meta2, 'dims': dims, 'page_num': page_num}
            if tmp_dir is not None:
                with open(os.path.join(tmp_dir, pdf_name) + f'_{page_num}.pkl', 'wb') as wf:
                    pickle.dump(obj, wf)
                objs.append((tmp_dir, pdf_name, page_num))
            else:
                objs.append(obj)
        return objs


def process_page(filepath, pdfid, session, client):
    raise PendingDeprecationWarning("process_page will be deprecated in a future release. Transition to Ingest API")
    page_num = int(os.path.basename(filepath))
    try:
        img = Image.open(filepath)
    except Image.DecompressionBombError as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e

    width, height = img.size
    with open(filepath, 'rb') as bimage:
        bstring = bimage.read()
        bytesio = io.BytesIO(bstring)
        img = resize_png(bytesio)
        # Convert it back to bytes
        resize_bytes_stream = io.BytesIO()
        img.save(resize_bytes_stream, format='PNG')
        resize_bytes_stream.seek(0)
        resize_bytes = resize_bytes_stream.read()
        resize_bytes = base64.b64encode(resize_bytes).decode('ASCII')
        resize_bytes_stream.seek(0)
        bstring = resize_bytes_stream.getvalue()
#        page = Page(pdf_id=pdfid, page_width=width, page_height=height, page_number=page_num, bytes=bstring)
        page = Page(pdf_id=pdfid, page_width=width, page_height=height, page_number=page_num)
        session.add(page)
    # Because PDFs can be very large (100+ pages), this transaction can become very large if we batch all pages together
    # As a result I'm committing on a per page basis.
    # In the future, this should be batched, and rollback should be fixed to properly remove already committed pages
    try:
        session.commit()
        session.refresh(page)
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()
    obj = {'bytes': resize_bytes, 'page_id': page.id}
    return obj

def load_images_from_dir(idir, pdf_name, image_ref):
    raise PendingDeprecationWarning("load_images_from_dir will be deprecated in a future release. Transition to Ingest API")
    files = glob.glob(f'{idir}/*')
    objs = []
    for filepath in files:
        page_num = int(os.path.basename(filepath))
        try:
            img = Image.open(filepath)
        except Image.DecompressionBombError as e:
            logger.error(str(e), exc_info=True)
            raise e
        width, height = img.size
        with open(filepath, 'rb') as bimage:
            bstring = bimage.read()
            bytesio = io.BytesIO(bstring)
            img = resize_png(bytesio)
            # Convert it back to bytes
            if image_ref:
                img.save(filepath, format='PNG')
                obj = {'bytes': filepath, 'page_id': str(uuid.uuid4()), 'pdf_name': pdf_name, 'page_num': page_num, 'pdf_name': pdf_name}
                objs.append(obj)
            else:
                resize_bytes_stream = io.BytesIO()
                img.save(resize_bytes_stream, format='PNG')
                resize_bytes_stream.seek(0)
                resize_bytes = resize_bytes_stream.read()
                resize_bytes = base64.b64encode(resize_bytes).decode('ASCII')
                resize_bytes_stream.seek(0)
                obj = {'bytes': resize_bytes, 'page_id': str(uuid.uuid4()), 'pdf_name': pdf_name, 'page_num': page_num, 'pdf_name': pdf_name}
                objs.append(obj)
    return objs



def ingest(obj, conn_str=None):
    raise PendingDeprecationWarning("ingest will be deprecated in a future release. Transition to Ingest API")
    if conn_str is None:
        engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    else:
        engine = create_engine(conn_str)
    Session = sessionmaker()
    Session.configure(bind=engine)
    pdf_file = base64.b64decode(obj['pdf'].encode())
    if 'dataset_id' not in obj or 'pdf_name' not in obj:
        raise Exception('Malformed input, no dataset_id or pdf_name in input')

    dataset_id = obj['dataset_id']
    pdf_name = obj['pdf_name']

    # Don't ingest if we have this pdf_name in this dataset_id
    session = Session()
    n = session.query(Pdf).filter(Pdf.pdf_name == pdf_name, Pdf.dataset_id == dataset_id).count()
    if n > 0:
        logger.info("Already ingested this PDF to this dataset_id!")
        return None

    with tempfile.NamedTemporaryFile() as tf, tempfile.TemporaryDirectory() as td:
        tf.write(pdf_file)
        tf.seek(0)
        try:
            meta, dims = parse_pdf(tf.name)
        except Exception as e:
            logger.error(str(e), exc_info=True)
            raise Exception('Parsing error', str(e))
        if meta is not None:
            meta = meta.to_dict()
            meta = json.dumps(meta)
            meta = json.loads(meta)
            dims = list(dims)
        pdf_id = uuid.uuid4()
        subprocess.run(['gs', '-dBATCH',
                              '-dNOPAUSE',
                              '-sDEVICE=png16m',
                              '-dGraphicsAlphaBits=4',
                              '-dTextAlphaBits=4',
                              '-r600',
                              f'-sOutputFile="{td}/%d"',
                              tf.name
                        ])
        pdf = Pdf(pdf_name=pdf_name, meta=meta, meta_dimension=dims, name=pdf_id, dataset_id=dataset_id)
        session.add(pdf)
        session.commit()
        session.refresh(pdf)
        session.close()
        fnames = glob.glob(f'{td}/*')
        pdfns = [pdf.id] * len(fnames)
        payload = list(zip(fnames, pdfns))
        logger.info('Starting page requests')
        objs = []
        client = get_client()
        for fname, pdfn in payload:
            obj = process_page(fname, pdfn, Session(), client)
            objs.append(obj)
        processed = [client.submit(pp, obj, resources={'process': 1}, priority=8) for obj in objs]
        detected_processed = client.map(detect, processed, resources={'GPU': 1}, priority=9)
        postprocessed_pages = client.map(postprocess_page, detected_processed, resources={'process': 1}, priority=10)
        job_keys = [p.key for p in postprocessed_pages]
        for p in postprocessed_pages:
            fire_and_forget(p)
        logger.info(f'Processed {len(fnames)} pages')
        return job_keys


