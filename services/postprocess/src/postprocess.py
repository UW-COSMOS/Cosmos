"""
Post processing on detected objects
"""
import pymongo
from pymongo import MongoClient
import time
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from joblib import Parallel, delayed
import click
from xgboost_model.inference import run_inference, PostprocessException
import os

def load_detected_pages(db, buffer_size):
    """
    """
    current_docs = []
    for doc in db.propose_pages.find({'postprocess': None, 'ocr': True}, no_cursor_timeout=True):
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs

def do_skip(page, client):
    db = client.pdfs
    coll = db.postprocess_pages
    return coll.count_documents({'pdf_name': page['pdf_name'], 'page_num': page['page_num']}, limit=1) != 0


def postprocess(db_insert_fn, num_processes, weights_pth, skip):
    logging.info('Starting post-processing over detected objects')
    start_time = time.time()
    client = MongoClient(os.environ["DBCONNECT"])
    logging.info(f'Connected to client: {client}.')
    db = client.pdfs
    for batch in load_detected_pages(db, 100):
        logging.info('Loaded next batch. Running postprocessing')
        try:
            pages = Parallel(n_jobs=num_processes)(delayed(run_inference)(page, weights_pth) for page in batch)
        except PostprocessException as e:
            logging.error(f'Postprocessing error in referenced page: {e.page}')
            logging.error(f'Original Exception: {e.original_exception}')
            continue

        db_insert_fn(pages, client)
    end_time = time.time()
    logging.info(f'Exiting post-processing. Time up: {end_time - start_time}')

def mongo_insert_fn(objs, client):
    db = client.pdfs
    for obj in objs:
        try:
            result = db.propose_pages.update_one({'_id': obj['_id']},
                                             {'$set':
                                                {
                                                    'pp_detected_objs': obj['pp_detected_objs'],
                                                    'postprocess': True
                                                }
                                             }, upsert=False)
            logging.info(f'Updated result: {result}')
        except pymongo.errors.WriterError as e:
            logging.error(f'Document write error: {e}\n Document id: obj["_id"]')

@click.command()
@click.argument("num_processes")
@click.argument("weights_pth")
@click.option('--skip/--no-skip')
def click_wrapper(num_processes, weights_pth, skip):
	postprocess(mongo_insert_fn, int(num_processes), weights_pth, skip)

if __name__ == '__main__':
	click_wrapper()
