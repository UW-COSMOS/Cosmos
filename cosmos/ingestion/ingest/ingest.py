import uuid
#import celery
from ingest.schema import Pdf, Page, PageObject
import json
import uuid
import tempfile
from ingest.pdf_extractor import parse_pdf
from ingest.preprocess import resize_png
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
from ingest.process_page import process_page as pp
from ingest.process_page import postprocess_page
from ingest.detect import detect
from dask.distributed import get_client, secede, rejoin, fire_and_forget

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
#CELERY_BROKER = os.environ.get('CELERY_BROKER')
#CELERY_BACKEND = os.environ.get('CELERY_BACKEND')
#
#app = celery.Celery('ingest', broker=CELERY_BROKER, backend=CELERY_BACKEND)

def process_page(filepath, pdfid, session, client):
    page_num = int(os.path.basename(filepath))
    try:
        img = Image.open(filepath)
    except Image.DecompressionBombError as e:
        logging.error(str(e), exc_info=True)
        session.rollback()
        raise e

    width, height = img.size
    with open(filepath, 'rb') as bimage:
        bstring = bimage.read()
        bytesio = io.BytesIO(bstring)
        img = resize_png(bytesio)
        # Convert it back to bytes
        resize_bytes_stream = io.BytesIO()
        img.save(resize_bytes_stream, format='PNG')
        resize_bytes_stream.seek(0)
        resize_bytes = resize_bytes_stream.read()
        resize_bytes = base64.b64encode(resize_bytes).decode('ASCII')
        resize_bytes_stream.seek(0)
        bstring = resize_bytes_stream.getvalue()
#        page = Page(pdf_id=pdfid, page_width=width, page_height=height, page_number=page_num, bytes=bstring)
        page = Page(pdf_id=pdfid, page_width=width, page_height=height, page_number=page_num)
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
        raise e
    finally:
        session.close()
    obj = {'bytes': resize_bytes, 'page_id': page.id}
    return obj

def pdf_to_images(obj):
    pdf_file = base64.b64decode(obj['pdf'].encode())
    if 'dataset_id' not in obj or 'pdf_name' not in obj:
        raise Exception('Malformed input, no dataset_id or pdf_name in input')
    dataset_id = obj['dataset_id']
    pdf_name = obj['pdf_name']
    with tempfile.NamedTemporaryFile() as tf, tempfile.TemporaryDirectory() as td:
        tf.write(pdf_file)
        tf.seek(0)
        try:
            meta, dims = parse_pdf(tf.name)
        except Exception as e:
            logger.error(str(e), exc_info=True)
            raise Exception('Parsing error', str(e))
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
        files = glob.glob(f'{td}/*')
        objs = []
        for filepath in files:
            page_num = int(os.path.basename(filepath))
            try:
                img = Image.open(filepath)
            except Image.DecompressionBombError as e:
                logging.error(str(e), exc_info=True)
                raise e
            width, height = img.size
            with open(filepath, 'rb') as bimage:
                bstring = bimage.read()
                bytesio = io.BytesIO(bstring)
                img = resize_png(bytesio)
                # Convert it back to bytes
                resize_bytes_stream = io.BytesIO()
                img.save(resize_bytes_stream, format='PNG')
                resize_bytes_stream.seek(0)
                resize_bytes = resize_bytes_stream.read()
                resize_bytes = base64.b64encode(resize_bytes).decode('ASCII')
                resize_bytes_stream.seek(0)
                obj = {'bytes': resize_bytes, 'page_id': str(uuid.uuid4()), 'pdf_name': pdf_name, 'page_num': page_num}
                objs.append(obj)
        return objs

def ingest(obj, conn_str=None):
    if conn_str is None:
        engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    else:
        engine = create_engine(conn_str)
    Session = sessionmaker()
    Session.configure(bind=engine)
    pdf_file = base64.b64decode(obj['pdf'].encode())
    if 'dataset_id' not in obj or 'pdf_name' not in obj:
        raise Exception('Malformed input, no dataset_id or pdf_name in input')

    dataset_id = obj['dataset_id']
    pdf_name = obj['pdf_name']

    # Don't ingest if we have this pdf_name in this dataset_id
    session = Session()
    n = session.query(Pdf).filter(Pdf.pdf_name == pdf_name, Pdf.dataset_id == dataset_id).count()
    if n > 0:
        logging.info("Already ingested this PDF to this dataset_id!")
        return None

    with tempfile.NamedTemporaryFile() as tf, tempfile.TemporaryDirectory() as td:
        tf.write(pdf_file)
        tf.seek(0)
        try:
            meta, dims = parse_pdf(tf.name)
        except Exception as e:
            logger.error(str(e), exc_info=True)
            raise Exception('Parsing error', str(e))
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
        pdf = Pdf(pdf_name=pdf_name, meta=meta, meta_dimension=dims, name=pdf_id, dataset_id=dataset_id)
        session.add(pdf)
        session.commit()
        session.refresh(pdf)
        session.close()
        fnames = glob.glob(f'{td}/*')
        pdfns = [pdf.id] * len(fnames)
        payload = list(zip(fnames, pdfns))
        logger.info('Starting page requests')
        objs = []
        client = get_client()
        for fname, pdfn in payload:
            obj = process_page(fname, pdfn, Session(), client)
            objs.append(obj)
        processed = [client.submit(pp, obj, resources={'process': 1}, priority=8) for obj in objs]
        detected_processed = client.map(detect, processed, resources={'GPU': 1}, priority=9)
        postprocessed_pages = client.map(postprocess_page, detected_processed, resources={'process': 1}, priority=10)
        job_keys = [p.key for p in postprocessed_pages]
        for p in postprocessed_pages:
            fire_and_forget(p)
        logger.info(f'Processed {len(fnames)} pages')
        return job_keys


