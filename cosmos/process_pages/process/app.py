"""
Route to handle decomposition of pages into page objects
"""
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
from PIL import Image
from process.proposals.connected_components import get_proposals
from process.detection.src.preprocess import pad_image
from process.detection.src.infer import run_inference, get_model
from process.ocr.ocr import run as ocr
from process.postprocess.xgboost_model.inference import run_inference as postprocess
import yaml
import joblib

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logging.getLogger("pdfminer").setLevel(logging.DEBUG)


class ProcessPage(object):
    """
    Process a single page, decomposing the page into objects
    """

    def __init__(self, config_pth, classes_pth, weights_pth, postprocess_weights_pth, device_str, use_db=True):
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
        self.model = get_model(config_pth, weights_pth, device_str)
        self.model_config = config_pth
        self.device_str = device_str
        self.postprocess_model = joblib.load(postprocess_weights_pth)
        if use_db:
            self.engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@database/cosmos')
            self.Session = sessionmaker()
            self.Session.configure(bind=engine)

        with open(classes_pth) as stream:
            self.classes = yaml.load(stream)["classes"]

    def query_pageid(self, id):
        """
        Query with a page id, return a page
        :param id: Page ID
        :return: Page object
        """
        session = self.Session()
        return session.query(Page).filter(Page.id == id).one()

    def commit_objs(self, objs, page):
        """
        Commit a list of objects. Raises an exception if commit fails.
        :param objs: list of objects to commit
        :param page: Page to set as foreign key
        """
        session = self.Session()
        ids = []
        for obj in objs:
            pobj = PageObject(bytes=obj['bstring'], content=obj['content'], bounding_box=obj['bb'], cls=obj['cls'])
            pobj.page = page
            ids.append(pobj.id)
            session.add(pobj)
        try:
            session.commit()
        except Exception as e:
            session.rollback()
            raise e
        return ids

    def on_post(self, req, resp):
        """
        Post route to process a single page
        :param req: Request object. Must contain an id as a query parameter, or else returns 400 code
        :param resp: Response object
        """
        inputs = falcon.uri.parse_query_string(req.query_string)
        if 'id' not in inputs:
            resp.status = falcon.HTTP_400
            return
        result = self.query_pageid(inputs['id'])
        img = Image.open(io.BytesIO(result['bytes'])).convert('RGB')
        coords = get_proposals(img)
        padded_img = pad_image(img)
        obj = {'id': inputs['id'], 'img': padded_img, 'proposals': coords}
        detected_objs = run_inference(self.model, [obj], self.model_config, self.device_str)['1']
        tess_df, objects = ocr(padded_img, detected_objs)
        objects = postprocess(self.postprocess_model, self.classes, objects)
        page_objs = []
        for obj in objects:
            bb, cls, text = obj
            feathered_bb = [max(bb[0]-2, 0), max(bb[1]-2, 0),
                            min(bb[2]+2, 1920), min(bb[3]+2, 1920)]
            cropped_img = img.crop(feathered_bb)
            bytes_stream = io.BytesIO()
            cropped_img.save(bytes_stream, format='PNG')
            bstring = bytes_stream.getvalue()
            bb = json.loads(json.dumps(bb))
            page_objs.append({'bstring': bstring, 'bb': bb, 'content': text, 'cls': cls})
        try:
            ids = self.commit_objs(page_objs, result)
            resp.status = falcon.HTTP_200
            resp.data = ids
        except Exception as e:
            logging.warning(f'{e}')
            resp.status = falcon.HTTP_500
            return


api = application = falcon.API()
pp = ProcessPage('process/detection/src/model_config.yaml','process/postprocess/classes.yaml', 'process/detection/weights/model_weights.pth','process/postprocess/weights/pp_model_weights.pth', 'cuda', use_db=False)
api.add_route('/process', pp)

