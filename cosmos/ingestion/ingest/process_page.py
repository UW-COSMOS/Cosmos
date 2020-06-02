
"""
Route to handle decomposition of pages into page objects
"""
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
from ingest.process.proposals.connected_components import get_proposals
from ingest.process.detection.src.preprocess import pad_image
from ingest.process.ocr.ocr import run as ocr
from ingest.process.ocr.group_cls import check_overlap_bb
from ingest.process.postprocess.xgboost_model.inference import run_inference as postprocess
from ingest.process.postprocess.pp_rules import apply_rules as postprocess_rules
from dask.distributed import get_worker

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

def process_page(inputs):
    if 'bytes' not in inputs:
        raise Exception('Invalid input, bytes not in input')
    result = inputs['bytes']
    page_id = inputs['page_id']

    img = Image.open(io.BytesIO(base64.b64decode(result.encode('ASCII')))).convert('RGB')
    coords = get_proposals(img)
    padded_img = pad_image(img)
    byteImgIO = io.BytesIO()
    padded_img.save(byteImgIO, "PNG")
    byteImgIO.seek(0)
    byteImg = byteImgIO.read()
    strimg = base64.b64encode(byteImg).decode('ASCII')
    obj = {'id': '0', 'pad_img': strimg, 'proposals': coords, 'page_id': page_id}
    return obj


def commit_objs(objs, page_id, session):
    """
    Commit a list of objects. Raises an exception if commit fails.
    :param objs: list of objects to commit
    :param page: Page to set as foreign key
    """
    ids = []
    pobjs = []
    for obj in objs:
        pobj = PageObject(init_cls_confidences=obj['detect_objects'], bytes=obj['bstring'], content=obj['content'], bounding_box=obj['bb'], cls=obj['cls'], page_id=page_id, confidence=obj['confidence'], pp_rule_cls=obj['pp_rule_cls'])
        session.add(pobj)
        pobjs.append(pobj)
    session.commit()
    for obj in pobjs:
        session.refresh(obj)
        ids.append(obj.id)
    return ids


def postprocess_page(obj):
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'ProcessPlugin' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise Exception('No process plugin registered')
        engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
        Session = sessionmaker()
        Session.configure(bind=engine)
        session = Session()
        padded_img = Image.open(io.BytesIO(base64.b64decode(obj['pad_img'].encode('ASCII')))).convert('RGB')
        detected_objs = obj['detected_objs']
        softmax_detect_objects = obj['softmax_objs']
        tess_df, detect_objects = ocr(padded_img, detected_objs)
        pageid = obj['page_id']

        if detect_objects is not None:
            objects = postprocess(dp.postprocess_model, dp.classes, detect_objects)
            objects = postprocess_rules(objects)
            page_objs = []
            for obj in objects:
                bb, cls, text, score, pp_rule_cls = obj
                logging.info(f"class: {cls}, score: {score}")
                feathered_bb = [max(bb[0]-2, 0), max(bb[1]-2, 0),
                                min(bb[2]+2, 1920), min(bb[3]+2, 1920)]
                cropped_img = padded_img.crop(feathered_bb)
                bytes_stream = io.BytesIO()
                cropped_img.save(bytes_stream, format='PNG', optimize=True)
                bstring = bytes_stream.getvalue()
                bb = json.loads(json.dumps(bb))
                s_detect_objects = json.dumps(softmax_detect_objects)
                s_detect_objects = json.loads(s_detect_objects)
                d_objects = None
                max_score = 0
                for dobj in s_detect_objects:
                    check_bb = dobj[0]
                    if check_overlap_bb(check_bb, bb):
                        d_score = dobj[1][0][0]
                        if d_score > max_score:
                            max_score = d_score
                            d_objects = dobj[1]
                if d_objects is None:
                    raise Exception('Something very wrong, bounding boxes are not aligned')

                page_objs.append({'detect_objects': d_objects, 'bstring': bstring, 'bb': bb, 'content': text, 'cls': cls, 'confidence': score, 'pp_rule_cls' : pp_rule_cls})
            ids = commit_objs(page_objs, pageid, session)
            return {'ids': ids}
        else:
            return {'ids': []}
    except UnidentifiedImageError as e:
        logger.error(str(e), exc_info=True)
        raise e
    except Exception as e:
        logger.error(str(e), exc_info=True)
        raise e
    finally:
        session.close()




