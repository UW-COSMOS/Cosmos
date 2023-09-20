
"""
Process an input directory of .pdf files, creating .parquet files and images into an output directory
uses an intermediate directory for page PNG files and pickled page info.
Can be invoked to return intermediate PNG files and pickled page info instead of parquet files.
Also can invoked with the intermediate PNG files supplied as input with the process continuing from there.
This script assumes it is invoked inside the docker image "uwcosmos/cosmos-ingestion" or equivalent
as a HTCondor job. See propose.sub and parquet.sub for suggested HTCondor submit files.
"""
import os
import sys
import pickle
import json
import io
import subprocess
import glob
import time
import yaml # for xgboost classes

# for sleep
#import time

from PIL import Image
from pathlib import Path
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

# for logging
from datetime import datetime

from ingest.utils.visualize import write_regions
from ingest.process.proposals.connected_components import get_proposals, get_lp_proposals
from ingest.utils.pdf_extractor import parse_pdf
from ingest.utils.preprocess import resize_png
from ingest.utils.table_extraction import TableLocationProcessor
from ingest.process.detection.src.preprocess import pad_image
from ingest.process.detection.src.infer import get_model, run_inference
from ingest.process.detection.src.torch_model.train.data_layer.sql_types import Base

#for my_run_inference
#from ingest.process.detection.src.torch_model.model.utils.config_manager import ConfigManager
#import torch
#from ingest.process.detection.src.torch_model.inference.inference import InferenceHelper
#from ingest.process.detection.src.torch_model.inference.data_layer.inference_loader import InferenceLoader
#from ingest.process.detection.src.utils.ingest_images import ImageDB

#for regroup & pool_text phases
from ingest.process.ocr.ocr import group_cls, _pool_text_meta, _placeholder_map

# for post processing
from ingest.process.postprocess.xgboost_model.inference import run_inference as postprocess
from ingest.process.postprocess.pp_rules import apply_rules as postprocess_rules

# for ConfigManager class
from ingest.process.detection.src.torch_model.model.utils.config_manager import ConfigManager

# aggregation
from ingest.utils.normalize_text import normalize_text
import pandas as pd
from ingest.process.aggregation.aggregate import aggregate_router

# for xgboost_postprocess
from xgboost import XGBClassifier

def tlog(msg):
    now = datetime.now().strftime('%X.%f')
    print(f'{now} {msg}')

def tlog_flush(msg):
    now = datetime.now().strftime('%X.%f')
    print(f'{now} {msg}', flush=True)

# helper for creating debugging infofiles in json format
def debug_file(obj, json_path):
    with open(json_path, 'w') as wf:
        json.dump(obj, wf)
    return json_path

""" create progress file and tlog the same message, also flush output """
def set_progress(filename, msg):
    now = datetime.now().strftime('%X.%f')
    line = f'{now} {msg}'
    with open(filename, 'w') as wf:
        print(line, file=wf, flush=True)
    print(line, flush=True)

""" delete progress file if it exists """
def clear_progress(filename):
    try:
        os.remove(filename)
    except FileNotFoundError:
        return False
    return True

""" check if progress file exists """
def check_progress(filename):
    return os.path.isfile(filename)

# given an page image filename, return the page number
def get_page_num(image_name):
    return int(image_name.split("_")[-1])

# given an page image filename, return a sort key
def get_page_key(image_name):
    return 1000+int(image_name.split("_")[-1])

# load an image file and convert  to RGB, if unable to load return None
def load_image(image_path):
    with open(image_path, 'rb') as bimage:
        bstring = bimage.read()
    bytesio = io.BytesIO(bstring)
    try:
        img = Image.open(bytesio).convert('RGB')
    except Exception as e:
        return None
    return img

# copy metadata and scale data in the copy the given page by the given scale factors
def pull_meta(meta, limit, page_num, scale_w, scale_h):
    meta2 = None
    if meta is not None:
        meta2 = meta.copy()
        meta2.loc[meta2.page == (page_num-1), 'x1'] = meta2.x1 * scale_w
        meta2.loc[meta2.page == (page_num-1), 'x2'] = meta2.x2 * scale_w
        meta2.loc[meta2.page == (page_num-1), 'y1'] = meta2.y1 * scale_h
        meta2.loc[meta2.page == (page_num-1), 'y2'] = meta2.y2 * scale_h
        meta2 = meta2.to_dict()
        meta2 = json.dumps(meta2)
        meta2 = json.loads(meta2)
    return meta2

"""
    Main processing function for the pages in a single PDF file.

    This function processes a input list of page .PNG files and creates a dict for each page which it saves
    as a pickle file. the JUST_PROPOSE environment variable controls whether this function does GPU inference
    processing or not.  When JUST_PROPOSE is not set, this function will read existing page pickles and use the
    proposals in those pickles, if no proposal is found it will do the proposal.  It will then invoke the GPU
    inference model and save the result in the pickle and also return it.

    When JUST_PROPOSE is set, it will do the proposal for each page, overwriting any proposal data in the
    input pickle and return the proposal results.

    If any of the pages cannot be processed, False will be returned along with the objs and infofiles created
    up to that point.

    args:
        filename      - relative path the the PDF file being processed, typically {pdf_dir)/{dataset_id}.pdf
        pages         - list of relative paths to the page PNG files for the PDF, typically {page_info_dir}/{dataset_id}_{page_num}.png
        page_info_dir - relative path to intermediate directory for pickles, padded images and other infofiles
        meta          - PDF metadata or None
        limit         - PDF page size in pixels or None
        model         - GPU inference model or None
        model_config  - GPU inference model or None
        device_str    - the value 'cpu', 'cuda' or None

    return: (success, objs, infofiles)
        success       - a boolean value that indicates overall success or failure
        objs          - a list of dict objects, one for each input page in the pages list
        infofiles     - a dict of temporary files created in the {page_info_dir}
                        dict key is filename and value is the type of file, one of
                           'page'   - page PNG files
                           'pad'    - scaled and padded page PNG files.  these are needed by aggregation
                           'pickle' - contents of a page dict saved as a pickle file
                           'json'   - selected page processing results saved as json data

    The meta, and limit values can be None if the processing is skipping the propose step
    The model, model_config and device_str values can be None if processing is just doing the propose step

"""
def process_pages(filename, pages, page_info_dir, meta, limit, model, model_config, device_str):

    just_propose = os.environ.get("JUST_PROPOSE") is not None

    tlog(f"create database engine")
    engine = create_engine('sqlite:///:memory:', echo=False)
    Session = sessionmaker()
    Session.configure(bind=engine)
    Base.metadata.create_all(engine)
    session = Session()

    pdf_name = os.path.basename(filename)
    dataset_id = Path(filename).stem
    objs = []    # working dict, saved as pickle
    infofiles = {} # files we may want to delete before we exit.

    tlog_flush(f'processing {pdf_name} and files matching it from {page_info_dir}')

    for image_path in pages:

        infofiles[image_path] = 'page'

        image_name = os.path.basename(image_path)
        try:
            page_num = get_page_num(image_name)
        except ValueError:
            raise Exception(f'cannot extract page number from {image_path}')

        page_name = f'pdf_{page_num}'
        tlog(f'processing {page_name} from {image_path}')

        # load or create the per-page dict
        pkl_path = f'{image_path}.pkl'
        if (os.path.isfile(pkl_path)):
            with open(pkl_path, 'rb') as rf:
                obj = pickle.load(rf)
        else:
            obj = {'dataset_id': dataset_id, 'pdf_name': pdf_name, 'page_num': page_num, 'page_id': image_name}

        # the inference model uses this as a key for the output data
        model_id = '0'
        obj['id'] = model_id
        obj['page_path'] = image_path

        # the processing looks a resized-padded image.
        # but the proposal step only looks at the resized image
        pad_img_path = f'{image_path}_pad'

        proposals = obj.get('proposals')
        make_proposals = proposals is None or just_propose

        # if we already have proposals, then we don't need to load the non-padded image
        # we can just load the padded image.  If there is no padded image then we
        # load the page image and resize/pad it.
        if os.path.isfile(pad_img_path) and not make_proposals:
            padded_img = load_image(pad_img_path)
            if padded_img is None:
                tlog_flush(f'ERROR: failed to read padded image {pad_img_path} - aborting this pdf')
                return (False, objs, infofiles)

            orig_w = obj['orig_w']
            orig_h = obj['orig_h']
        else:
            # get the page image
            img = load_image(image_path)
            if img is None:
                tlog(f'ERROR: failed to read image {image_path} - aborting this pdf')
                return (False, objs, infofiles)

            tlog(f'{image_name} is {img.size} resizing to 1920')
            orig_w, orig_h = img.size
            obj['orig_w'] = orig_w
            obj['orig_h'] = orig_h

            # the model wants square images of size 1920, so we resize and square it now
            img, img_size = resize_png(img, return_size=True)
            tlog(f'{page_name} is now {img_size}')
            padded_img = pad_image(img)
            tlog(f'{page_name} padded to {padded_img.size}')
            # the padding operation is
            #want_image_size = 1920
            #d_w = want_image_size - w
            #d_h = want_image_size - h
            #padding = (0,0,d_w, d_h)
            #padded_img = ImageOps.expand(img, padding, fill="#fff")

            # the aggregation code also needs access to the padded image
            # so save this file for later
            padded_img.save(pad_img_path, "PNG")
            infofiles[pad_img_path] = 'pad'

        # refresh the path to the padded image in case we are resuming on a different machine
        obj['pad_img'] = pad_img_path

        # if meta/limit is supplied, store/refresh the page relevent meta info
        if limit is not None:
            obj['pdf_limit'] = limit
        else:
            limit = obj.get('pdf_limit')

        if limit is not None:
            orig_w = limit[2]
            orig_h = limit[3]
        else:
            tlog(f'ERROR: parsing of {pdf_name} was unsuccessful - aborting this pdf')
            return (False, objs, infofiles)

        if meta is not None:
            w,h = img.size
            scale_w = w / orig_w
            scale_h = h / orig_h
            meta2 = pull_meta(meta, limit, page_num, scale_w, scale_h)
            dims = [0, 0, w, h]

            obj['meta'] = meta2
            obj['dims'] = dims

        # get proposed coords for use by the inference model
        if make_proposals:
            tlog(f'{page_name} get proposals')
            proposals = get_proposals(img)
            # tlog(f'Cosmos proposals:')
            # tlog(proposals)
            lp_proposals = get_lp_proposals(img, 0.5)
            # tlog(f'LayoutParser proposals:')
            # tlog(lp_proposals)
            #obj['proposals'] = proposals
            obj['proposals'] = lp_proposals
            if just_propose:
                pkl_path = f'{page_info_dir}/{image_name}.pkl'
                #tlog_flush(f'writing {page_name} proposals to {pkl_path}')
                tlog_flush(f'writing {page_name} lp_proposals to {pkl_path}')
                with open(pkl_path, 'wb') as wf:
                    pickle.dump(obj, wf)
                # tj's debugging stuff - write proposals and meta also as json
                # infofiles[debug_file(obj, f'{page_info_dir}/{image_name}.json')] = 'json'
                objs.append(obj)

                continue

        tlog(f'{page_name} invoke inference model')
        tlog(f'   proposals: {proposals}')

        #detect_obj = {'id': model_id, 'proposals': proposals, 'img': padded_img}
        detect_obj = {'id': model_id, 'proposals': lp_proposals, 'img': padded_img}
        detected_objs, softmax_detected_objs = run_inference(model, [detect_obj], model_config, device_str, session)

        tlog(f'{page_name} inference complete')

        detected = detected_objs[model_id]
        softmax = softmax_detected_objs[model_id]

        # save results and clear any lingering post-processing data
        # to indicate post-processing is still needed
        obj['detected_objs'] = detected
        obj['softmax_objs'] = softmax
        obj.pop('content', None)
        obj.pop('xgboost_content', None)
        obj.pop('rules_content', None)

        # put output pickles into an ouput directory
        pkl_path = f'{page_info_dir}/{image_name}.pkl'
        tlog_flush(f'writing {page_name} model results to {pkl_path}')
        with open(pkl_path, 'wb') as wf:
            pickle.dump(obj, wf)
        infofiles[pkl_path] = 'pickle'

        # tj's debugging stuff - also write model results to json files
        #infofiles[debug_file(obj, f'{page_info_dir}/{image_name}.json')] = 'json'
        #infofiles[debug_file(detected_objs, f'{page_info_dir}/{image_name}.detected.json')] = 'json'
        #infofiles[debug_file(softmax_detected_objs, f'{page_info_dir}/{image_name}.softmax.json')] = 'json'
        # ^^ tj's debugging stuff

        objs.append(obj)

    tlog(f'close database session')
    session.close()

    # return a list of temprary files to be returned or deleted
    return (True, objs, infofiles)

"""
    Post inference aggregation of the page metadata and creation of parquet files and png files needed by them

    This function expects to consume the intermediate files produced by the process_pages function.  It will
    create the resized,padded image files if they do not already exist in the {page_info_dir}, and then it will
    perform the post-processing, aggregation and parquet creation steps for the input pages.  parquet files
    will be written to {out_dir}, updated pickes and possibly padded page files are written to {page_info_dir}.

    Args:
        filename          - relative path the the PDF file being processed, typically {pdf_dir)/{dataset_id}.pdf
        pages             - list of relative paths to the page PNG files for the PDF, typically {page_info_dir}/{dataset_id}_{page_num}.png
        page_info_dir     - relative path to intermediate directory for pickles, padded images and other infofiles
        out_dir           - relative path to output parquet files and png files referenced therein
        postprocess_model - xgboost posprocessing model
        pp_classes        - 'CLASSES' from the model config
        aggregations      - list of aggregation categories, one parquet file is created for each entry in this list

    return: (success, objs, infofiles)
        success       - a boolean value that indicates overall success or failure
        objs          - a list of dict objects, one for each input page in the pages list
        infofiles     - a dict of temporary files created in the {page_info_dir}
                          key is filename and value is the type of file, one of
                           'pad'    - scaled and padded page PNG files.
                           'pickle' - contents of a page dict saved as a pickle file

"""
def aggregate_pages(filename, pages, page_info_dir, out_dir, postprocess_model, pp_classes, aggregations):

    use_text_normalization = True

    pdf_name = os.path.basename(filename)
    dataset_id = Path(filename).stem
    results = [] # result list, saved as parquet
    objs = []    # intermediate page dicts
    infofiles = {} # files we may want to delete before we exit.

    tlog(f'post-processing {pdf_name} and files matching it from {page_info_dir}')

    for image_path in pages:

        image_name = os.path.basename(image_path)
        try:
            page_num = get_page_num(image_name)
        except ValueError:
            raise Exception(f'cannot extract page number from {image_path}')

        page_name = f'pdf_{page_num}'
        pkl_path = f'{image_path}.pkl'
        tlog(f'post-processing {page_name} from {pkl_path}')

        # get or create the metadata (we need the proposals)
        #
        try:
            with open(pkl_path, 'rb') as rf:
                obj = pickle.load(rf)
        except Exception as e:
            tlog(f'ERROR: failed to read pickle {pkl_path} - aborting this pdf')
            return (False, objs, infofiles)

        # refresh the page path
        obj['page_path'] = image_path

        # we need the result of the inference model in order to proceed
        if 'detected_objs' in obj.keys():
            detected = obj['detected_objs']
        else:
            tlog_flush(f'ERROR: no detected_objs in {pkl_path} - aborting this pdf')
            return (False, objs, infofiles)

        # the aggregation code needs access to the padded image, so if
        # it does not exist, we need to make it now
        pad_img_path = f'{image_path}_pad'
        if not os.path.isfile(pad_img_path):
            img = load_image(image_path)
            if img is None:
                tlog_flush(f'ERROR: failed to read image {image_path} - aborting this pdf')
                return (False, objs, infofiles)

            img, img_size = resize_png(img, return_size=True)
            padded_img = pad_image(img)
            padded_img.save(pad_img_path, "PNG")

        # refresh the padded image path, and let the KEEP_INFO code know about it
        obj['pad_img'] = pad_img_path
        infofiles[pad_img_path] = 'pad'

        # --- post-processing work ---
        # regroup
        detected = group_cls(detected, 'Table', do_table_merge=True, merge_over_classes=['Figure', 'Section Header', 'Page Footer', 'Page Header'])
        detected = group_cls(detected, 'Figure')
        tlog(f'{page_name} regroup complete')

        # pool_text
        if "meta" in obj and obj['meta'] is not None:
            text_map = _pool_text_meta(obj['meta'], obj['dims'][3], detected, obj['page_num'])
        #elif not skip_ocr:
        #    text_map = _pool_text_ocr(image_path, detected)
        else:
            text_map = _placeholder_map(detected)
        obj['content'] = text_map
        tlog(f'{page_name} pool_text complete')

        # xgboost_postprocess
        xgboost_content = postprocess(postprocess_model, pp_classes, text_map)
        # remove empty strings returned from postprocess
        xgboost_content = [i for i in xgboost_content if i != '']
        obj['xgboost_content'] = xgboost_content
        tlog(f'{page_name} xgboost_postprocess complete')

        # rules postprocess
        rules_content = postprocess_rules(xgboost_content)
        obj['rules_content'] = rules_content
        tlog(f'{page_name} rules_postprocess complete')

        # put output pickles into an ouput directory
        tlog_flush(f'writing {page_name} post-processing results to {pkl_path}')
        with open(pkl_path, 'wb') as wf:
            pickle.dump(obj, wf)
        infofiles[pkl_path] = 'pickle'

        # add to the list of intermediate objects that we return
        objs.append(obj)

        # aggregate
        for ind, c in enumerate(obj['content']):
            bb, cls, text = c
            if use_text_normalization:
                text = normalize_text(text)
            scores, classes = zip(*cls)
            scores = list(scores)
            classes = list(classes)
            postprocess_cls = postprocess_score = None
            if 'xgboost_content' in obj:
                _, postprocess_cls, _, postprocess_score = obj['xgboost_content'][ind]
                if 'rules_content' in obj:
                    _, postprocess_cls, _, postprocess_score = obj['rules_content'][ind]
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


        tlog(f'done post-processing {pkl_path}\n')

    if len(results) > 0:
        tlog(f'creating parquet files')
        # create a parquet file
        result_df = pd.DataFrame(results)
        result_df['detect_cls'] = result_df['classes'].apply(lambda x: x[0])
        result_df['detect_score'] = result_df['scores'].apply(lambda x: x[0])
        result_df.to_parquet(os.path.join(out_dir, f'{dataset_id}.parquet'), engine='pyarrow', compression='gzip')
        for aggregation in aggregations:
            aggregate_df = aggregate_router(result_df, aggregate_type=aggregation, write_images_pth=out_dir)
            name = f'{dataset_id}_{aggregation}.parquet'
            aggregate_df.to_parquet(os.path.join(out_dir, name), engine='pyarrow', compression='gzip')

    # return a list of temprary files to be returned or deleted
    return (True, objs, infofiles)

"""
    main processing loop for PDF files, called by main after command line parsing.

    This function processes each PDFs in the {pdf_dir} and invokes the other functions on each
    PDF one at a time.  What processing happens depends on environment variables JUST_PROPOSE,
    SKIP_AGGREGATION, and JUST_AGGREGATION, and on the presence or absence of *.complete files
    in the intermediate processing directory.

    Args:
        pdf_dir       - directory for input PDF files
        page_info_dir - in/out directory for intermediate files such as page PNGs and pickles
        out_dir       - directory for output parquet files and the png files used therein

    returns
        stats         - dict of counts of processing successes and failures

"""
def main_process(pdf_dir, page_info_dir, out_dir):
    resume_mode = False # True if may be resuming from an previous run
    # counts of stuff we do or failed to do
    stats = {'gs':0, 'gs_error':0,
             'process':0, 'process_error':0,
             'aggregate':0, 'aggregate_error':0,
             'pdfs':0, 'attempted':0, 'succeeded':0, 'finished':0, 'already':0 }

    # fetch processing options from the environment
    keep = {} # types of intermediate files we want to return (i.e. not delete)
    keep_info_list = os.environ.get("KEEP_INFO")
    if keep_info_list is not None:
        for t in keep_info_list.split(",") : keep[t] = True

    # these indicate that we should not process to completion, but return
    # partial results on purpose
    just_propose = os.environ.get("JUST_PROPOSE") is not None
    skip_aggregation = os.environ.get("SKIP_AGGREGATION") is not None
    just_aggregation = os.environ.get("JUST_AGGREGATION") is not None

    # create our output and page_info directory if they do not already exist
    # if the page_info directory exists, we will check for {pdf}.*.complete files
    # to know what parts of the processing we can skip
    tlog(f'create {out_dir}')
    Path(out_dir).mkdir(parents=True, exist_ok=True)
    pi = Path(page_info_dir)
    if pi.is_dir():
        resume_mode = True
    else:
        tlog(f'create {page_info_dir}')
        pi.mkdir(parents=True, exist_ok=True)
    tlog(f'--- requested keep={keep} ---')

    # inference model config
    model_config = os.environ.get("MODEL_CONFIG")
    weights_pth = os.environ.get("WEIGHTS_PTH")
    device_str = 'cpu'
    if model_config is None or weights_pth is None:
        if not just_propose and not just_aggregation:
            tlog('abort because environment has no MODEL_CONFIG or WEIGHTS_PTH')
            sys.exit(1)
    else:
        # use a gpu for the model if we have one and we are actually doing the model
        cuda_visible_dev = os.environ.get("CUDA_VISIBLE_DEVICES")
        if cuda_visible_dev is not None:
            tlog(f'using gpu={cuda_visible_dev}')
            device_str = 'cuda'

    # xgboost post-processing config
    pp_weights_path = os.environ.get("PP_WEIGHTS_PTH")
    aggregation_list = os.environ.get("AGGREGATIONS")
    if pp_weights_path is None or aggregation_list is None:
        if not skip_aggregation and not just_propose:
            tlog('abort because environment has no PP_WEIGHTS_PATH or AGGREGATIONS')
            sys.exit(1)
        skip_aggregation = True

    # interpret the partial processing options, just_propose, skip_aggregation and just_aggregation
    if just_propose:
        skip_aggregation = True
        aggregations = None
        postprocess_model = None
        pp_classes = None
        model = None
        keep['pickle'] = True
        keep['page'] = True
        stats['partial'] = 'proposed'
    else:
        if skip_aggregation:
            aggregations = None
            postprocess_model = None
            pp_classes = None
            keep['pickle'] = True
            keep['page'] = True
            stats['partial'] = 'processed'
        else:
            aggregations = aggregation_list.split(",")
            tlog(f"--- aggregations={aggregations} ---")

            tlog(f'loading xgboost model weights={pp_weights_path}')
            postprocess_model = XGBClassifier()
            postprocess_model.load_model(pp_weights_path)

            #cfg = ConfigManager(model_config)
            with open(model_config) as stream:
                pp_classes = yaml.load(stream, yaml.Loader)["CLASSES"]
            tlog(f'--- pp_classes={pp_classes} ---')

        if just_aggregation:
            model = None
        else:
            tlog(f'loading inference model config={model_config} weights={weights_pth}, {device_str}')
            tlog(os.environ["LD_LIBRARY_PATH"])
            import torch
            tlog(torch.__file__)
            tlog(torch.cuda.is_available())
            model = get_model(model_config, weights_pth, device_str)

    # ------- main PDF processing loop --------
    # we will process all of the *.pdf files from the input dir
    #
    pdfs = glob.glob(f'{pdf_dir}/*.pdf')
    tlog(f'processing *.pdf files from {pdf_dir}')
    for filename in pdfs:

        success = True
        stats['pdfs'] += 1

        pdf_name = os.path.basename(filename)
        dataset_id = Path(filename).stem

        # names of the various partial progress files
        progress_filename_pages = f'{page_info_dir}/{pdf_name}.pages.complete'
        progress_filename_propose = f'{page_info_dir}/{pdf_name}.propose.complete'
        progress_filename_process = f'{page_info_dir}/{pdf_name}.process.complete'
        progress_filename_parquet = f'{page_info_dir}/{pdf_name}.parquet.complete'

        if check_progress(progress_filename_parquet):
            stats['already'] += 1
            continue

        stats['attempted'] += 1

        create_pages = not resume_mode
        pages = glob.glob(f'{page_info_dir}/{pdf_name}_*[0-9]')
        if len(pages) == 0:
            create_pages = True
        else:
            create_pages = not check_progress(progress_filename_pages)
            if not create_pages:
                tlog(f'skipping page print because {progress_filename_pages} exists')

        if create_pages:
            clear_progress(progress_filename_pages)
            clear_progress(progress_filename_propose)
            clear_progress(progress_filename_process)
            clear_progress(progress_filename_parquet)

            timelimit = 10*60 # wait no longer than 10 minutes
            tlog(f'print pages for {pdf_name} with timeout={timelimit}')
            args = ['gs', '-dBATCH',
                          '-dNOPAUSE',
                          '-sDEVICE=png16m',
                          '-dGraphicsAlphaBits=4',
                          '-dTextAlphaBits=4',
                          '-r600',
                          f'-sOutputFile="{page_info_dir}/{pdf_name}_%d"',
                          filename]
            try:
                result = subprocess.run(args, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT,
                                        timeout=timelimit, check=True)
                pages = glob.glob(f'{page_info_dir}/{pdf_name}_*[0-9]')
                num = len(pages)
                set_progress(progress_filename_pages, f'printed {num} pages for {pdf_name}')
                stats['gs'] += 1
            except subprocess.SubprocessError as e:
                pages = []
                stats['gs_error'] += 1
                if isinstance(e, subprocess.TimeoutExpired):
                    tlog_flush(f'ERROR: timeout printing {pdf_name} pages - skipping this pdf')
                else:
                    tlog_flush(f'ERROR: ghostscript error: {e.returncode} for {pdf_name}')
                success = False

        # lets process the pages in page number order, does it matter? probably not...
        pages.sort(key=get_page_key)

        infofiles = {}
        # process each of the page files we just created or were transfered in
        if success and not just_aggregation:
            check = progress_filename_process
            if (just_propose):
               check = progress_filename_propose
            success = check_progress(check)
            if success:
                tlog_flush(f'skipping processing because {check} exists')
            else:
                # process_pages needs the parse_pdf metadata if processing will be
                # or might be doing the propose step.  This is a bit awkward but we
                # want to support both the workflow where we propose and process each
                # page in a single job, and also the workflow where we propose
                # in a separate job, and process in a later job.  In the second case
                # the inference processing step does not need to re-parse the pdf.
                meta = None
                limit = None
                if not just_propose and check_progress(progress_filename_propose):
                    tlog(f'skipping parse_pdf because {progress_filename_propose} exists')
                else:
                    tlog(f'parse pdf {filename}')
                    meta, limit = parse_pdf(filename)

                success, objs, infofiles = process_pages(filename, pages, page_info_dir,
                                                            meta, limit,
                                                            model, model_config, device_str)
                if success:
                    num = len(objs)
                    if just_propose:
                        set_progress(progress_filename_propose, f'just_propose for {num} pages for {pdf_name}')
                    else:
                        set_progress(progress_filename_process, f'processed {num} pages for {pdf_name}')
                    stats['process'] += 1
                else:
                    stats['process_error'] += 1
                    tlog_flush(f'failed to process {pdf_name}')

        if success and not skip_aggregation:
            success, objs, moar_infofiles = aggregate_pages(filename, pages, page_info_dir, out_dir,
                                                            postprocess_model, pp_classes, aggregations)
            infofiles.update(moar_infofiles)
            if success:
                stats['aggregate'] += 1
                set_progress(progress_filename_parquet, f'created parquet files for {pdf_name}')
                stats['finished'] += 1
            else:
                stats['aggregate_error'] += 1
                tlog_flush(f'failed to aggregate {pdf_name}')

        # remove the infofiles created by processing that we have not been told to keep
        for file,typ in infofiles.items():
            if typ in keep:
                tlog_flush(f'   keep_info {typ} {file}')
            else:
                tlog_flush(f'          rm {typ} {file}')
                os.remove(file)

        if success:
            stats['succeeded'] += 1

    tlog_flush(f'Succeeded in processing {stats["succeeded"]} documents')
    return stats


def resize_files(out_dir):
    # resave the images as jpg
    files = glob.glob(os.path.join(out_dir,"*.png"))
    tlog(f"Converting {len(files)} snipped PNG files to JPG")
    for path in files:
        if os.path.exists(path.replace("png","jpg")): continue
        im = Image.open(path)
        im.save(path.replace("png","jpg"), quality=40, optimize=True)
        im.thumbnail((200,200))
        im.save(path.replace(".png","_thumb.jpg"), quality=40, optimize=True)
        os.remove(path)
    tlog("PNG to JPG converstion complete.")

def extract_tables(pdf_dir, out_dir):
    tlog(f"Extracting tables")
    files = glob.glob(os.path.join(out_dir,"*tables*.parquet"))
    for fin in files:
        tlog(fin)
        proc = TableLocationProcessor(fin, pdf_dir + "/", "", os.path.join(out_dir, "tables/"))
        _ = proc.extract_pickles()
    tlog(f"Tables extracted.")

if __name__ == '__main__':
    if len(sys.argv) < 4:
        print("Usage: python make_parquet.py <pdf-dir> <page_info_dir> <output_dir>")
        sys.exit(1)

    (pdf_dir, page_info_dir, out_dir) = sys.argv[1:]

    stats = main_process(pdf_dir, page_info_dir, out_dir)
    tlog(f'--- stats={stats} ---')

    #tlog(f'sleep(60) to make memory tracking more accurate when processing takes < 60 sec')
    #time.sleep(60)

    # report on what we did and decide what our exit code will be
    attempted = stats['attempted']
    succeeded = stats['succeeded']
    finished = stats['finished']
    already = stats['already']
    failed = attempted - succeeded
    if already > 0:
        tlog(f'Skipped {already} pdf files because of completion files.')
    work = stats.get('partial')
    if work is None:
        tlog(f'Created {finished} parquet data sets out of {attempted} attempted.')
    else:
        tlog(f'Successfully {work} {succeeded} pdf files out of {attempted} attempted.')
    resize_files(out_dir)
    extract_tables(out_dir)
    if failed > 0:
        tlog(f'Failed to process {failed} pdf files.')
        sys.exit(1)
