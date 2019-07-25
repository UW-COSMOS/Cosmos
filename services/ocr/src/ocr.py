"""
Run OCR over docs, also merge
"""

import json
import pymongo
from pymongo import MongoClient
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import time
import io
from PIL import Image
import re
from joblib import Parallel, delayed
import click
import pickle
from group_cls import group_cls
import pytesseract
import pandas as pd

def load_pages(db, buffer_size):
    """
    """
    current_docs = []
    for doc in db.detect_pages.find().batch_size(buffer_size):
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs


def do_skip(page, client):
    db = client.pdfs
    coll = db.ocr_pages
    return coll.count_documents({'pdf_name': page['pdf_name'], 'page_num': page['page_num']}, limit=1) != 0


def process_page(page):
    img = Image.open(io.BytesIO(page['resize_bytes']))
    width, height = img.size
    tess_df = pytesseract.image_to_data(img, output_type=pytesseract.Output.DATAFRAME)
    tess_df['bottom'] = tess_df['top'] + tess_df['height']
    tess_df['right'] = tess_df['left'] + tess_df['width']
    if 'detected_objs' not in page:
        return (None, f'This page has not had detected or has no detected objects: {page["_id"]}')
    if len(page['detected_objs']) == 0:
        return (None, f'This page has no detected objs: {page["_id"]}')
    detect_objs = page['detected_objs']
    obj_str_list = []
    for obj in detect_objs:
        if len(obj) != 2:
            return (None, f'This page\'s detected objects are not of length 2')
        bb, scr_cls = obj
        tl_x, tl_y, br_x, br_y = bb
        obj_ocr = tess_df.loc[(tess_df['bottom'] <= br_y) & (tess_df['top'] >= tl_y) &
                          (tess_df['left'] >= tl_x) & (tess_df['right'] <= br_x)]
        words = obj_ocr['text']
        word_list = []
        for ind, word in words.iteritems():
            word = str(word)
            #word = strip_regx.sub('', word)
            if not word:
                continue
            word_list.append(word)
        word_list = [word for word in word_list if word != 'nan']
        word_dump = ' '.join(word_list)
        obj_str_list.append(word_dump)
    bbs, scrs = zip(*detect_objs)
    full_zip = list(zip(bbs, scrs, obj_str_list))
    page['ocr_detected_objs'] = full_zip
    tess_df = tess_df.to_dict()
    tess_df = json.dumps(tess_df)
    tess_df = json.loads(tess_df)
    page['ocr_df'] = tess_df
    return (page, None)


def ocr_scan(db_insert_fn, num_processes, skip):
    logging.info('Starting ocr over pages')
    start_time = time.time()
    client = MongoClient(os.environ['DBCONNECT'])
    logging.info(f'Connected to client: {client}')
    db = client.pdfs
    for batch in load_pages(db, num_processes):
        if skip:
            batch = [page for page in batch if not do_skip(page, client)]
            if len(batch) == 0:
                continue
        #pages = [process_page(page, db) for page in batch]
        pages = Parallel(n_jobs=num_processes)(delayed(process_page)(page) for page in batch)
        pages, errs = zip(*pages)
        for err in errs:
            if err is None:
                continue
            logging.debug(err)

        pages = [page for page in pages if page is not None]
        #objs = [o for p in pages for o in p]
        db_insert_fn(pages, client)


def mongo_insert_fn(objs, client):
    db = client.pdfs
    if len(objs) == 0:
        logging.info("Batch has no ocr pages")
        return
    result = db.ocr_pages.insert_many(objs)
    logging.info(f"Inserted results: {result}")


@click.command()
@click.argument('num_processes')
@click.option('--skip/--no-skip')
def click_wrapper(num_processes, skip):
    ocr_scan(mongo_insert_fn, int(num_processes), skip)


if __name__ == '__main__':
    click_wrapper()



