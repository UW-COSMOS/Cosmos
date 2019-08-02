"""
Extract objects
"""
import pandas as pd
import json
import pymongo
from pymongo import MongoClient
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import time
import io
from PIL import Image as img
import re
from joblib import Parallel, delayed
import click
import pickle
from table_extractions import extract_table_from_obj


def load_pages(db, buffer_size):
    """
    """
    current_docs = []
    
    for doc in db.postprocess_pages.find().batch_size(buffer_size):
    
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs


def do_skip(page, client):
    # TODO
    return False


def extract_objs(page):
    if 'pp_detected_objs' not in page:
        return (None, f'This page has not had postprocessing done on it')
    if len(page['pp_detected_objs']) == 0:
        return (None, f'No detected objs on page: {page["_id"]}')
    detected_objs = page['pp_detected_objs']
    # Sanity check that filters objects not of length 3
    detected_objs = [obj for obj in detected_objs if len(obj) == 3]
    #l = group_cls(detected_objs, 'Table', do_table_merge=True, merge_over_classes=['Figure', 'Section Header', 'Page Footer', 'Page Header'])
    #l = group_cls(l, 'Figure')
    #page['merged_objs'] = l
    objs = []
    strip_regx = re.compile('[^a-zA-Z]') # Strip all non alphabet characters
    tess_df = pd.DataFrame(page['ocr_df'])
    for obj in detected_objs:
        bb, cls, score = obj

        table_df = None
        if cls == 'Table':
            pdf_name = page['pdf_name']
            page_num = str(page['page_num'])
            coords = bb
            logging.info(f'Found a table: {pdf_name}, {page_num}')
            table_df = extract_table_from_obj(pdf_name, page_num, coords)

        tl_x, tl_y, br_x, br_y = bb
        obj_ocr = tess_df.loc[(tess_df['bottom'] <= br_y) & (tess_df['top'] >= tl_y) &
                          (tess_df['left'] >= tl_x) & (tess_df['right'] <= br_x)]
        feathered_bb = [max(bb[0]-2, 0), max(bb[1]-2, 0),
                        min(bb[2]+2, 1920), min(bb[3]+2, 1920)]
        
        #cropped_img = img.crop(feathered_bb)
        #bytes_stream = io.BytesIO()
        #cropped_img.save(bytes_stream, format='PNG')
        #bstring = bytes_stream.getvalue()
        
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
        obj_ocr = obj_ocr.to_dict()
        obj_ocr = json.dumps(obj_ocr)
        obj_ocr = json.loads(obj_ocr)
        final_obj = {'bounding_box': bb,                                   #'bytes': bstring,
                     'page_ocr_df': obj_ocr, 'class': cls, 'score': score,
                     'pdf_name': page['pdf_name'], 'page_num': page['page_num'], 'content': word_dump, 'table_df': table_df}
        objs.append(final_obj)
    return (objs, None)


def extract_scan(db_insert_fn, num_processes, skip):
    logging.info('Starting object extraction over pages')
    start_time = time.time()
    client = MongoClient(os.environ['DBCONNECT'])
    logging.info(f'Connected to client: {client}')
    db = client.pdfs
    for batch in load_pages(db, num_processes):
        #print(batch[0]['detected_objs'])
        if skip:
            batch = [page for page in batch if not do_skip(page, client)]
            if len(batch) == 0:
                continue
        #pages = [process_page(page, db) for page in batch]
        pages = Parallel(n_jobs=num_processes)(delayed(extract_objs)(page) for page in batch)
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
    result = db.ocr_objs_test.insert_many(objs)
    logging.info(f"Inserted results: {result}")


@click.command()
@click.argument('num_processes')
@click.option('--skip/--no-skip')
def click_wrapper(num_processes, skip):
    extract_scan(mongo_insert_fn, int(num_processes), skip)


if __name__ == '__main__':
    click_wrapper()
