"""
Detect objects over pages
"""

import pymongo
from pymongo import MongoClient
import os
import logging
import datetime
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import time
import io
from preprocess import pad_image
import base64
from infer import run_inference
import click
from joblib import Parallel, delayed


def preprocess_page(page):
    bytesio = io.BytesIO(page['resize_bytes'])
    img = pad_image(bytesio)
    padded_bytes_stream = io.BytesIO()
    img.save(padded_bytes_stream, format='PNG')
    padded_bytes = padded_bytes_stream.getvalue()
    page['padded_bytes'] = padded_bytes
    return page


def load_pages(db, buffer_size):
    """
    """
    current_docs = []
    for doc in db.propose_pages.find().batch_size(buffer_size):
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs


def do_skip(page, client):
    db = client.pdfs
    coll = db.detect_pages
    return coll.count_documents({'pdf_name': page['pdf_name'], 'page_num': page['page_num']}, limit=1) != 0


def pages_detection_scan(config_pth, weights_pth, num_processes, db_insert_fn, skip):
    logging.info('Starting detection over papers')
    start_time = time.time()
    client = MongoClient(os.environ["DBCONNECT"])
    logging.info(f'Connected to client: {client}.')
    db = client.pdfs
    for batch in load_pages(db, 500):
        if skip:
            batch = [page for page in batch if not do_skip(page, client)]
            if len(batch) == 0:
                continue
        pages = Parallel(n_jobs=num_processes)(delayed(preprocess_page)(page) for page in batch)
        detected_objs = run_inference(pages, config_pth, weights_pth, os.environ["DEVICE"])
        for page in pages:
            page_id = str(page['_id'])
            page['detected_objs'] = detected_objs[page_id]
            del page['padded_bytes']
            
        db_insert_fn(pages, client)

    end_time = time.time()
    logging.info(f'Exiting detection. Time up: {end_time - start_time}')

def mongo_insert_fn(objs, client):
    db = client.pdfs
    try:
        result = db.detect_pages.insert_many(objs)
        logging.info(f"Inserted results: {result}")
    except pymongo.errors.BulkWriteError:
        for obj in objs:
            result = db.detect_pages.replace_one({'_id': obj['_id']}, obj, upsert=True)
            logging.info(f"Inserted result: {result}")


@click.command()
@click.argument('config_path')
@click.argument('weights_path')
@click.argument('num_processes')
@click.option('--skip/--no-skip')
def click_wrapper(config_path, weights_path, num_processes, skip):
    pages_detection_scan(config_path, weights_path, int(num_processes), mongo_insert_fn, skip)


if __name__ == '__main__':
    click_wrapper()


