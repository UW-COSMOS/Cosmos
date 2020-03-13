
"""
Route to handle decomposition of pages into page objects
"""
import copy
import base64
import uuid
import celery
from .schema import Pdf, Page, PageObject
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
from ingest.process.postprocess.xgboost_model.inference import run_inference as postprocess
import yaml
import joblib
import requests

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

CELERY_BROKER = os.environ.get('CELERY_BROKER')
CELERY_BACKEND = os.environ.get('CELERY_BACKEND')

app = celery.Celery('postprocess_page', broker=CELERY_BROKER, backend=CELERY_BACKEND)

class ProcessPageTask(celery.Task):

    def __init__(self):
        cfg_path = '/ingest/process/configs/model_config.yaml'
        postprocess_weights_pth = '/ingest/process/weights/pp_model_weights.pth'
        self.postprocess_model = None# joblib.load(postprocess_weights_pth)
        self.classes_pth = '/ingest/process/configs/classes.yaml'
        self.classes=None

def commit_objs(objs, page_id, session):
    """
    Commit a list of objects. Raises an exception if commit fails.
    :param objs: list of objects to commit
    :param page: Page to set as foreign key
    """
    ids = []
    pobjs = []
    for obj in objs:
        pobj = PageObject(bytes=obj['bstring'], content=obj['content'], bounding_box=obj['bb'], cls=obj['cls'], page_id=page_id)
        session.add(pobj)
        pobjs.append(pobj)
    session.commit()
    for obj in pobjs:
        session.refresh(obj)
        ids.append(obj.id)
    return ids


#@app.task(base=ProcessPageTask, bind=True, name='postprocess_page', queue='process_q')
@app.task(bind=True, name='postprocess_page', queue='postprocess_q')
def postprocess_page(self, obj):
    cfg_path = '/ingest/process/configs/model_config.yaml'
    postprocess_weights_pth = '/ingest/process/weights/pp_model_weights.pth'
    self.postprocess_model = None# joblib.load(postprocess_weights_pth)
    self.classes_pth = '/ingest/process/configs/classes.yaml'
    self.classes=None
    if self.postprocess_model is None:
        self.postprocess_model = joblib.load(postprocess_weights_pth)
    if self.classes is None:
        with open(self.classes_pth) as stream:
            self.classes = yaml.load(stream)["classes"]

    logger.error('WHAT WHAT WHAT WHAT')
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    try:
        padded_img = Image.open(io.BytesIO(base64.b64decode(obj['pad_img'].encode('ASCII')))).convert('RGB')
        detected_objs = obj['detected_objs']
        tess_df, objects = ocr(padded_img, detected_objs)

        if objects is not None:
            objects = postprocess(self.postprocess_model, self.classes, objects)
            page_objs = []
            for obj in objects:
                bb, cls, text = obj
                feathered_bb = [max(bb[0]-2, 0), max(bb[1]-2, 0),
                                min(bb[2]+2, 1920), min(bb[3]+2, 1920)]
                cropped_img = img.crop(feathered_bb)
                bytes_stream = io.BytesIO()
                cropped_img.save(bytes_stream, format='PNG', optimize=True)
                bstring = bytes_stream.getvalue()
                bb = json.loads(json.dumps(bb))
                page_objs.append({'bstring': bstring, 'bb': bb, 'content': text, 'cls': cls})
            ids = self.commit_objs(page_objs, obj['page_id'], session)
            return {'ids': ids}
        else:
            return {'ids': []}
    except UnidentifiedImageError as e:
        logger.error(str(e), exc_info=True)
        raise e
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()




