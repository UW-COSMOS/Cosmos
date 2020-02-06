import uuid
import falcon
from .schema import Pdf, Page
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


import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@database/cosmos')
Session = sessionmaker()
Session.configure(bind=engine)

class PreprocessedPDF(object):

    def on_get(self, req, resp):
        resp.body = 'Hello world!'
        resp.status = falcon.HTTP_200

    def request_process(self, id):
        url = 'http://process_page:8000'
        result = requests.post(url, data = {'id': id})

    def on_post(self, req, resp):
        session = Session()
        
        pdf_file = req.bounded_stream.read()
        with tempfile.NamedTemporaryFile() as tf, tempfile.TemporaryDirectory() as td:
            tf.write(pdf_file)
            try:
                meta, dims = parse_pdf(tf.name)
            except Exception as e:
                logging.debug(e)
                resp.status = falcon.HTTP_400
                return
            meta = meta.to_dict()
            meta = json.dumps(meta)
            meta = json.loads(meta)
            dims = list(dims)
            pdf_id = uuid.uuid4()
            pdf = Pdf(bytes=pdf_file, meta=meta, meta_dimension=dims, name=pdf_id, dataset_id=0)
            session.add(pdf)
            subprocess.run(['gs', '-dBATCH',
                                  '-dNOPAUSE',
                                  '-sDEVICE=png16m',
                                  '-dGraphicsAlphaBits=4',
                                  '-dTextAlphaBits=4',
                                  '-r600',
                                  f'-sOutputFile="{td}/%d"',
                                  tf.name
                            ])
            page_ids = []
            for f in glob.glob(f'{td}/*'):
                page_num = int(os.path.basename(f))
                img = Image.open(f)
                width, height = img.size
                with open(f, 'rb') as bimage:
                    bstring = bimage.read()
                    bytesio = io.BytesIO(bstring)
                    img = resize_png(bytesio)
                    # Convert it back to bytes
                    resize_bytes_stream = io.BytesIO()
                    img.save(resize_bytes_stream, format='PNG')
                    resize_bytes = resize_bytes_stream.getvalue()
                    page = Page(bytes=resize_bytes, page_width=width, page_height=height, page_number=page_num)
                    page.pdf = pdf
                    session.add(page)
            try:
                session.commit()
            except Exception as e:
                logging.warning(f'{e}')
                session.rollback()
                resp.status = falcon.HTTP_400
                return
            with ThreadPoolExecutor(max_workers=50) as pool:
                print(list(pool.map(self.request_process, page_ids)))

            resp.status = falcon.HTTP_200


api = application = falcon.API()
pre_pdf = PreprocessedPDF()
api.add_route('/preprocess', pre_pdf)

