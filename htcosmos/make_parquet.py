
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
from ingest.process.proposals.connected_components import get_proposals
from ingest.utils.pdf_extractor import parse_pdf
from ingest.utils.preprocess import resize_png
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


def process_pages(filename, page_info_dir, out_dir, meta, limit, model, model_config, device_str, postprocess_model, pp_classes, aggregations):

    use_text_normalization = True
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
    results = [] # result dict, saved as parquet

    infofiles = {} # files we may want to delete before we exit.

    names = glob.glob(f'{page_info_dir}/{pdf_name}_*[0-9]')

    tlog(f'processing {pdf_name} and files matching it from {page_info_dir}')

    for image_path in names:

        infofiles[image_path] = 'page'

        image_name = os.path.basename(image_path)
        try:
            page_num = int(image_name.split("_")[-1])
        except ValueError:
            raise Exception(f'{image_path}')

        page_name = f'pdf_{page_num}'
        tlog(f'processing {page_name} from {image_path}')

        # get the page image
        with open(image_path, 'rb') as bimage:
            bstring = bimage.read()
        bytesio = io.BytesIO(bstring)
        try:
            img = Image.open(bytesio).convert('RGB')
        except Exception as e:
            #logger.error(str(e), exc_info=True)
            #logger.error(f'Image opening error pdf: {pdf_name}')
            return []

        tlog(f'{image_name} is {img.size} resizing to 1920')
        orig_w, orig_h = img.size

        # get or create the metadata (we need the proposals)
        #
        pkl_path = f'{image_path}.pkl'
        if (os.path.isfile(pkl_path)):
            with open(pkl_path, 'rb') as rf:
                obj = pickle.load(rf)
        else:
            obj = {'orig_w': orig_w, 'orig_h': orig_h, 'dataset_id': dataset_id, 'pdf_name': pdf_name, 'page_num': page_num, 'page_id': image_name}
        obj['page_path'] = image_path

        # the inference model uses this as a key for the output data
        id = '0'
        obj['id'] = id

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

        # the aggregation code needs access to the padded image
        pad_img_path = f'{image_path}_pad'
        padded_img.save(pad_img_path, "PNG")
        infofiles[pad_img_path] = 'pad'
        obj['pad_img'] = pad_img_path

        if limit is not None:
           obj['pdf_limit'] = limit
        else:
            limit = obj.get('pdf_limit')

        if limit is not None:
            orig_w = limit[2]
            orig_h = limit[3]

        if meta is not None:
            w,h = img.size
            scale_w = w / orig_w
            scale_h = h / orig_h
            meta2 = pull_meta(meta, limit, page_num, scale_w, scale_h)
            dims = [0, 0, w, h]

            obj['meta'] = meta2
            obj['dims'] = dims

        # get proposed coords for detection
        proposals = obj.get('proposals')
        if proposals is None:
            tlog(f'{page_name} get proposals')
            proposals = get_proposals(img)
            obj['proposals'] = proposals
            if just_propose:
                pkl_path = f'{page_info_dir}/{image_name}.pkl'
                tlog(f'writing {page_name} proposals to {pkl_path}')
                with open(pkl_path, 'wb') as wf:
                    pickle.dump(obj, wf)
                # tj's debugging stuff - also write to json files
                json_path = f'{page_info_dir}/{image_name}.json'
                tlog(f'also writing {page_name} pickles as json')
                with open(json_path, 'w') as wf:
                    json.dump(obj, wf)
                infofiles[json_path] = 'json'
                continue


        tlog(f'{page_name} invoke inference model')
        tlog(f'   proposing: {proposals}')

        detect_obj = {'id': id, 'proposals': proposals, 'img': padded_img}
        detected_objs, softmax_detected_objs = run_inference(model, [detect_obj], model_config, device_str, session)

        tlog(f'{page_name} inference complete')
        #tlog(f'--- detected_objs={detected_objs} ---')
        #tlog(f'--- softmax_detected_objs={softmax_detected_objs} ---')

        detected = detected_objs[id]
        softmax = softmax_detected_objs[id]

        #tlog(f'--- detected={detected} ---')
        #tlog(f'--- softmax={softmax} ---')

        # regroup
        detected = group_cls(detected, 'Table', do_table_merge=True, merge_over_classes=['Figure', 'Section Header', 'Page Footer', 'Page Header'])
        detected = group_cls(detected, 'Figure')
        tlog(f'{page_name} regroup complete')

        # save results
        obj['detected_objs'] = detected
        obj['softmax_objs'] = softmax

        # pool_text
        meta_df = obj['meta']
        if meta_df is not None:
            text_map = _pool_text_meta(meta_df, obj['dims'][3], detected, obj['page_num'])
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
        pkl_path = f'{page_info_dir}/{image_name}.pkl'
        tlog(f'writing {page_name} model results to {pkl_path}')
        with open(pkl_path, 'wb') as wf:
            pickle.dump(obj, wf)
        infofiles[pkl_path] = 'pickle'

        # tj's debugging stuff - also write model results to json files
        json_path = f'{page_info_dir}/{image_name}.json'
        tlog(f'also saving pickle for {page_name} as json')
        with open(json_path, 'w') as wf:
            json.dump(obj, wf)
        infofiles[json_path] = 'json'

        json_path = f'{page_info_dir}/{image_name}.detected.json'
        with open(json_path, 'w') as wf:
            json.dump(detected_objs, wf)
        infofiles[json_path] = 'json'
        json_path = f'{page_info_dir}/{image_name}.softmax.json'
        with open(json_path, 'w') as wf:
            json.dump(softmax_detected_objs, wf)
        infofiles[json_path] = 'json'
        # ^^ tj's debugging stuff

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

        tlog(f'done processing {image_path}\n')

    if len(results) > 0:
        tlog(f'creating parquet files')
        # create a parquet file
        result_df = pd.DataFrame(results)
        result_df['detect_cls'] = result_df['classes'].apply(lambda x: x[0])
        result_df['detect_score'] = result_df['scores'].apply(lambda x: x[0])
        for aggregation in aggregations:
            aggregate_df = aggregate_router(result_df, aggregate_type=aggregation, write_images_pth=out_dir)
            name = f'{dataset_id}_{aggregation}.parquet'
            aggregate_df.to_parquet(os.path.join(out_dir, name), engine='pyarrow', compression='gzip')

    tlog(f'close database session')
    session.close()

    # return a list of temprary files to be returned or deleted
    return infofiles

if __name__ == '__main__':
    if len(sys.argv) < 4:
        print("Usage: python make_parquet.py <pdf-dir> <page_info_dir> <output_dir>")
        sys.exit(1)

    (pdf_dir, page_info_dir, out_dir) = sys.argv[1:]

    create_pages = True # assume we will be creating the page PNG files

    # fetch processing options from the environment
    just_propose = os.environ.get("JUST_PROPOSE") is not None
    keep = {}
    keep_info_list = os.environ.get("KEEP_INFO")
    if keep_info_list is not None:
        for t in keep_info_list.split(",") : keep[t] = True

    # create our output and page_info directory if they do not already exist
    # if the page_info directory exists, we will assume that the initial page
    # PNG files were transferred in and skip creating them.
    tlog(f'create {out_dir}')
    Path(out_dir).mkdir(parents=True, exist_ok=True)
    pi = Path(page_info_dir)
    if pi.is_dir():
        # TODO: check for empty info dir instead of assuming an existing dir is non-empty
        create_pages = False
    else:
        tlog(f'create {page_info_dir}')
        pi.mkdir(parents=True, exist_ok=True)
    tlog(f"--- keep={keep} ---")

    model_config = os.environ.get("MODEL_CONFIG")
    weights_pth = os.environ.get("WEIGHTS_PTH")
    device_str = 'cpu'
    cuda_visible_dev = os.environ.get("CUDA_VISIBLE_DEVICES")
    if cuda_visible_dev is not None:
        tlog(f"using gpu={cuda_visible_dev}")
        device_str = 'cuda'

    # xgboost config
    pp_weights_path = os.environ.get("PP_WEIGHTS_PTH")

    aggregation_list = os.environ.get("AGGREGATIONS")
    if just_propose:
        aggregations = None
        postprocess_model = None
        pp_classes = None
        model = None
        keep['pickle'] = True
        keep['page'] = True
    else:
        aggregations = aggregation_list.split(",")
        tlog(f"--- aggregations={aggregations} ---")

        tlog(f"loading xgboost model weights={pp_weights_path}")
        postprocess_model = XGBClassifier()
        postprocess_model.load_model(pp_weights_path)

        #cfg = ConfigManager(model_config)
        with open(model_config) as stream:
            pp_classes = yaml.load(stream, yaml.Loader)["CLASSES"]
        tlog(f"--- pp_classes={pp_classes} ---")

        tlog(f"loading inference model config={model_config} weights={weights_pth}")
        model = get_model(model_config, weights_pth, device_str)

    # we will process all of the *.pdf files from the input dir
    #
    pdfs = glob.glob(f'{pdf_dir}/*.pdf')
    tlog(f'processing *.pdf files from {pdf_dir}')
    for filename in pdfs:

        pdf_name = os.path.basename(filename)
        dataset_id = Path(filename).stem

        meta = None
        limit = None
        if create_pages:
            tlog(f'parse pdf {filename}')
            meta, limit = parse_pdf(filename)

            tlog(f'print pages for {pdf_name}')
            subprocess.run(['gs', '-dBATCH',
                            '-dNOPAUSE',
                            '-sDEVICE=png16m',
                            '-dGraphicsAlphaBits=4',
                            '-dTextAlphaBits=4',
                            '-r600',
                            f'-sOutputFile="{page_info_dir}/{pdf_name}_%d"',
                            filename], stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

        # process each of the page files we just created or were transfered in
        infofiles = process_pages(filename, page_info_dir, out_dir,
                                meta, limit,
                                model, model_config, device_str,
                                postprocess_model, pp_classes, aggregations)

        # remove the infofiles created by processing that we have not been told to keep
        for file,type in infofiles.items():
            if type in keep:
                tlog(f'   keep_info {type} {file}')
            else:
                tlog(f'          rm {type} {file}')
                os.remove(file)

    #tlog(f'sleep(60) to make memory tracking more accurate when processing takes < 60 sec')
    #time.sleep(60)

    tlog(f'done')

