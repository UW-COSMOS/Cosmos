"""
Route to handle decomposition of pages into page objects
"""
import base64
import uuid
import falcon
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
from process.proposals.connected_components import get_proposals
from process.detection.src.preprocess import pad_image
from process.ocr.ocr import run as ocr
from process.postprocess.xgboost_model.inference import run_inference as postprocess
import yaml
import joblib
import requests

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

class ProcessPage(object):
    """
    Process a single page, decomposing the page into objects
    """

    def __init__(self, config_pth, classes_pth, pp_model, use_db=True):
        """
        Set the models, the backend (GPU/CPU), and setup the database connection
        :param config_pth: Configuration path to attentive_rcnn model yaml config
        :param classes_pth: Configuration path to classes yaml path
        :param weights_pth: Path to attentive_rcnn weights file
        :param postprocess_weights_pth: Path to XGBoost model weights
        :param device_str: Device string, IE: 'cpu' or 'cuda'
        :param use_db: Option to configure the db (optional for testing)
        """
        super().__init__()
        self.pp_model = pp_model
        if use_db:
            self.engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
            self.Session = sessionmaker()
            self.Session.configure(bind=self.engine)

        with open(classes_pth) as stream:
            self.classes = yaml.load(stream)["classes"]


    def commit_objs(self, objs, page_id, session):
        """
        Commit a list of objects. Raises an exception if commit fails.
        :param objs: list of objects to commit
        :param page: Page to set as foreign key
        """
        ids = []
        pobjs = []
        for obj in objs:
            pobj = PageObject(bytes=obj['bstring'], content=obj['content'], bounding_box=obj['bb'], cls=obj['cls'], page_id=page_id, confidence=obj['confidence'])
            session.add(pobj)
            pobjs.append(pobj)
        session.commit()
        for obj in pobjs:
            session.refresh(obj)
            ids.append(obj.id)
        return ids

    def detect(self, payload):
        url = 'http://detect:8002/detect'
        success = False
        try:
            result = requests.post(url, json=payload, timeout=60)
        except requests.exceptions.Timeout as e:
            logger.error(str(e), exc_info=True)
            return None
        except requests.exceptions.ConnectionError as e:
            logger.error(str(e), exc_info=True)
            return None

        try:
            return result.json()
        except json.JSONDecodeError as e:
            logger.error(str(e), exc_info=True)
            return None

    def on_post(self, req, resp):
        """
        Post route to process a single page
        :param req: Request object. Must contain an id as a query parameter, or else returns 400 code
        :param resp: Response object
        """
        session = self.Session()
        try:
            inputs = req.media
            if 'bytes' not in inputs:
                resp.status = falcon.HTTP_400
                raise falcon.HTTPBadRequest('bytes field not in input JSON', 'bytes field not in input JSON')
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
            obj = {'id': '0', 'img': strimg, 'proposals': coords}
            detected_objs = self.detect(obj)
            if detected_objs is None:
                raise Exception('Detect objects failed')
            tess_df, objects = ocr(padded_img, detected_objs)

            if objects is not None:
                objects = postprocess(self.pp_model, self.classes, objects)
                page_objs = []
                for obj in objects:
                    bb, cls, text, score = obj
                    feathered_bb = [max(bb[0]-2, 0), max(bb[1]-2, 0),
                                    min(bb[2]+2, 1920), min(bb[3]+2, 1920)]
                    cropped_img = img.crop(feathered_bb)
                    bytes_stream = io.BytesIO()
                    cropped_img.save(bytes_stream, format='PNG', optimize=True)
                    bstring = bytes_stream.getvalue()
                    bb = json.loads(json.dumps(bb))
                    page_objs.append({'bstring': bstring, 'bb': bb, 'content': text, 'cls': cls, 'confidence': score})
                ids = self.commit_objs(page_objs, page_id, session)
                resp.status = falcon.HTTP_200
                resp.body = json.dumps({'ids': ids})
            else:
                resp.status = falcon.HTTP_200
                resp.body = json.dumps({'ids': []})
        except UnidentifiedImageError as e:
            logger.error(str(e), exc_info=True)
            raise falcon.HTTPBadRequest(str(e), str(e))
        except falcon.HTTPBadRequest as e:
            raise e
        except Exception as e:
            logger.error(str(e), exc_info=True)
            session.rollback()
            raise falcon.HTTPInternalServerError('Process Page Error', 'Something went wrong processing this page')
        finally:
            session.close()

cfg_path = '/process/configs/model_config.yaml'
postprocess_weights_pth = '/process/weights/pp_model_weights.pth'
postprocess_model = joblib.load(postprocess_weights_pth)

api = application = falcon.API()
pp = ProcessPage(cfg_path, '/process/configs/classes.yaml', postprocess_model, use_db=True)
api.add_route('/process', pp)

