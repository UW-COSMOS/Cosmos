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

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
bsz = int(os.environ['PAGE_BSZ'])
Session = sessionmaker()
Session.configure(bind=engine)


class PreprocessedPDF(object):

    def request_process(self, bstring, page_id):
        url = 'http://process_pages:8001/process'
        result = requests.post(url, json={'bytes': bstring, 'page_id': page_id})
        try:
            return result.json()
        except json.JSONDecodeError as e:
            logger.error(str(e), exc_info=True)
            return None

    def process_page(self, payload):
        session = Session()
        f, pdfid = payload
        page_num = int(os.path.basename(f))
        try:
            img = Image.open(f)
        except Image.DecompressionBombError as e:
            logging.error(str(e), exc_info=True)
            session.rollback()
            raise falcon.HTTPBadRequest('Decompression bomb possibly detected.', 'PDF images determined to be too large, possibly indicating a decompression bomb.')

        width, height = img.size
        with open(f, 'rb') as bimage:
            bstring = bimage.read()
            bytesio = io.BytesIO(bstring)
            img = resize_png(bytesio)
            # Convert it back to bytes
            resize_bytes_stream = io.BytesIO()
            img.save(resize_bytes_stream, format='PNG')
            resize_bytes_stream.seek(0)
            resize_bytes = resize_bytes_stream.read()
            resize_bytes = base64.b64encode(resize_bytes).decode('ASCII')
            page = Page(page_width=width, page_height=height, page_number=page_num)
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
            raise falcon.HTTPInternalServerError('Commit issue', f'{e}')
        finally:
            session.close()
        result = self.request_process(resize_bytes, page.id)
        return (page.id, result)


    def on_post(self, req, resp):
        obj = req.media
        pdf_file = base64.b64decode(obj['pdf'].encode())
        if 'dataset_id' not in obj or 'pdf_name' not in obj:
            resp.status = falcon.HTTP_400
            raise falcon.HTTPBadRequest('dataset_id or pdf_name not in request params', 'dataset_id or pdf_name not in request params')

        dataset_id = obj['dataset_id']
        pdf_name = obj['pdf_name']
        
        with tempfile.NamedTemporaryFile() as tf, tempfile.TemporaryDirectory() as td:
            tf.write(pdf_file)
            tf.seek(0)
            try:
                meta, dims = parse_pdf(tf.name)
            except Exception as e:
                logger.error(str(e), exc_info=True)
                description = f'{e}'
                raise falcon.HTTPInternalServerError('Parse error', description)
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
            session = Session()
            pdf = Pdf(pdf_name=pdf_name, meta=meta, meta_dimension=dims, name=pdf_id, dataset_id=dataset_id)
            session.add(pdf)
            session.commit()
            session.refresh(pdf)
            session.close()
            fnames = glob.glob(f'{td}/*')
            pdfns = [pdf.id] * len(fnames)
            payload = list(zip(fnames, pdfns))
            logger.info('Starting page requests')
            resps = []
            with ThreadPoolExecutor(max_workers=32) as pool:
                resps = list(pool.map(self.process_page, payload))
            logger.info(f'Processed {len(resps)} pages')

            body = {'pdf_id': pdf.id, 'pages': {}, 'missed_pages': []}
            for pid, r in resps:
                if r is None or 'ids' not in r:
                    body['missed_pages'].append(pid)
                else:
                    body['pages'][pid] = r['ids']
            resp.status = falcon.HTTP_200


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
api.add_route('/delete', delete_dataset)

