import uuid
import falcon
from .schema import Pdf, Page, PageObject
import json
import uuid
import tempfile
from .pdf_extractor import parse_pdf
from .preprocess import resize_png
import json
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import click
import os
import io
import subprocess
import glob
from PIL import Image
import requests
from concurrent.futures import ThreadPoolExecutor
import base64
import time
from ingest.ingest import ingest
from dask.distributed import Client, fire_and_forget
client = Client('scheduler:8786')

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
Session = sessionmaker()
Session.configure(bind=engine)

class PreprocessedPDF(object):

    def on_post(self, req, resp):
        obj = req.media
        if 'dataset_id' not in obj or 'pdf_name' not in obj or 'pdf' not in obj:
            resp.status = falcon.HTTP_400
            raise falcon.HTTPBadRequest('dataset_id or pdf_name not in request params', 'dataset_id or pdf_name not in request params')
        ingested_objs_future = client.submit(ingest, obj, resources={'process': 1}, priority=-10)
        key = ingested_objs_future.key
        fire_and_forget(ingested_objs_future)
        #pp_chain = (process_page.s() | detect.s())
        #task = (ingest.s() | dmap.s(callback=pp_chain) ).delay(obj)
        resp.status = falcon.HTTP_200
        result = {
            'status': 'success',
            'data': {
                'task_id': key
            }
        }
        resp.body = json.dumps(result)

class CheckStatus(object):
    def on_get(self, req, resp, task_id):
        #task_result = AsyncResult(task_id)
        #result = {'status': task_result.status, 'result': task_result.result}
        resp.status = falcon.HTTP_200
        resp.body = json.dumps({'status': 'SUCCESS', 'result': []})#json.dumps(result)


class DeleteDataset(object):
    def on_post(self, req, resp):
        session = Session()
        obj = req.media
        if 'dataset_id' not in obj:
            resp.status = falcon.HTTP_400
            raise falcon.HTTPBadRequest('dataset_id not in request params', 'dataset_id not in request params')

        dataset_id = obj['dataset_id']
        for pdf in session.query(Pdf).filter(Pdf.dataset_id == dataset_id):
            for page in session.query(Page).filter(Page.pdf_id == pdf.id):
                for pobj in session.query(PageObject).filter(PageObject.page_id == page.id):
                    session.delete(pobj)
                session.delete(page)
            session.delete(pdf)
            try:
                session.commit()
            except Exception as e:
                logger.error(str(e), exc_info=True)
                session.rollback()
                pass
        session.close()
        

api = application = falcon.API()
pre_pdf = PreprocessedPDF()
delete_dataset = DeleteDataset()
api.add_route('/preprocess', pre_pdf)
api.add_route('/status/{task_id}', CheckStatus())
api.add_route('/delete', delete_dataset)

