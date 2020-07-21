
"""
Route to handle decomposition of pages into page objects
"""
from pathlib import Path
from scipy.special import softmax
import copy
import base64
import uuid
from ingest.schema import Pdf, Page, PageObject
import uuid
import tempfile
import json
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import click
import os
import io
import subprocess
import glob
from PIL import Image, UnidentifiedImageError
from ingest.utils.visualize import write_regions
from ingest.process.proposals.connected_components import get_proposals
from ingest.process.detection.src.preprocess import pad_image
from ingest.process.ocr.ocr import run as ocr
from ingest.process.ocr.group_cls import check_overlap_bb
from ingest.process.postprocess.xgboost_model.inference import run_inference as postprocess
from ingest.process.postprocess.pp_rules import apply_rules as postprocess_rules
from dask.distributed import get_worker
import pickle

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.INFO)
logging.getLogger("pdfminer").setLevel(logging.DEBUG)
logging.getLogger("PIL").setLevel(logging.DEBUG)
logger = logging.getLogger()
logger.setLevel(logging.INFO)


def propose_and_pad(obj, visualize=False):
    tmp_dir, pdf_name, page_num = obj
    pkl_path = f'{os.path.join(tmp_dir, pdf_name)}_{page_num}.pkl'
    image_path = f'{os.path.join(tmp_dir, pdf_name)}_{page_num}'
    img = Image.open(image_path).convert('RGB')
    with open(pkl_path, 'rb') as rf:
        try:
            obj = pickle.load(rf)
        except EOFError as e:
            logging.error(e)
            logging.error(f'Pickle path: {pkl_path}')
            raise e
        except pickle.UnpicklingError as e:
            logging.error(e)
            logging.error(f'Pickle path: {pkl_path}')
            raise e
    coords = get_proposals(img)
    padded_img = pad_image(img)
    obj['id'] = '0'
    obj['proposals'] = coords
    if visualize:
        write_regions(image_path, coords)
    obj['page_id'] = f'{pdf_name}_{page_num}'
    obj['page_path'] = f'{tmp_dir}/{obj["page_id"]}'
    d = f'{tmp_dir}/{pdf_name}_{page_num}_pad'
    padded_img.save(d, "PNG")
    obj['pad_img'] = d
    with open(pkl_path, 'wb') as wf:
        pickle.dump(obj, wf)
    return pkl_path


def xgboost_postprocess(obj):
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'ProcessPlugin' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise ValueError('No process plugin registered')
    except Exception as e:
        logger.error(str(e), exc_info=True)
        raise e

    objects = obj['content']
    objects = postprocess(dp.postprocess_model, dp.classes, objects)
    obj['xgboost_content'] = objects
    return obj


def rules_postprocess(obj):
    objects = obj['xgboost_content']
    objects = postprocess_rules(objects)
    obj['rules_content'] = objects
    return obj
