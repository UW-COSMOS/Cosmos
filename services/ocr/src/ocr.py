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
from bson.objectid import ObjectId

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
    if 'detected_objs' not in page:
        return (None, f'No detected objs on page: {page["_id"]}')
    detected_objs = page['detected_objs']
    l = group_cls(detected_objs, 'Table', do_table_merge=True, merge_over_classes=['Figure', 'Section Header', 'Page Footer', 'Page Header'])
    l = group_cls(l, 'Figure')
    page['merged_objs'] = l
    img = Image.open(io.BytesIO(page['resize_bytes']))
    width, height = img.size
    tess_df = pytesseract.image_to_data(img, output_type=pytesseract.Output.DATAFRAME)
    tess_df['bottom'] = tess_df['top'] + tess_df['height']
    tess_df['right'] = tess_df['left'] + tess_df['width']
    #print(tess_df)
    objs = []
    for obj in l:
        bb, cls, score = obj
        tl_x, tl_y, br_x, br_y = bb
        obj_ocr = tess_df.loc[(tess_df['bottom'] <= br_y) & (tess_df['top'] >= tl_y) &
                          (tess_df['left'] >= tl_x) & (tess_df['right'] <= br_x)]
        feathered_bb = [max(bb[0]-2, 0), max(bb[1]-2, 0),
                        min(bb[2]+2, width), min(bb[3]+2, height)]
        cropped_img = img.crop(feathered_bb)
        bytes_stream = io.BytesIO()
        cropped_img.save(bytes_stream, format='PNG')
        bstring = bytes_stream.getvalue()
        obj_ocr = obj_ocr.to_dict()
        obj_ocr = json.dumps(obj_ocr)
        obj_ocr = json.loads(obj_ocr)
        final_obj = {'bounding_box': bb, 'bytes': bstring,
                     'page_ocr_df': obj_ocr, 'class': cls, 'score': score,
                     'pdf_name': page['pdf_name'], 'page_num': page['page_num']}
        objs.append(final_obj)
    return (objs, None)
    

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
        objs = [o for p in pages for o in p]
        db_insert_fn(objs, client)


def mongo_insert_fn(objs, client):
    db = client.pdfs
    result = db.ocr_objs.insert_many(objs)
    logging.info(f"Inserted results: {result}")


@click.command()
@click.argument('num_processes')
@click.option('--skip/--no-skip')
def click_wrapper(num_processes, skip):
    ocr_scan(mongo_insert_fn, int(num_processes), skip)


if __name__ == '__main__':
    click_wrapper()



