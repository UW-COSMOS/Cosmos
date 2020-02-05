"""
Add proposals
"""

import pymongo
from pymongo import MongoClient
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import time
import io
from connected_components import get_proposals
from PIL import Image
import re
from joblib import Parallel, delayed
import click
import pickle


def is_picklable(obj):
    """
    Helpful function to debug whether objects are pickleable (requirement for multiprocessing)
    """
    try:
      pickle.dumps(obj)

    except pickle.PicklingError:
      return False
    return True


def load_docs(db, buffer_size):
    """
    """
    current_docs = []
    for doc in db.pages.find():
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs


def run_page(page, db_insert_fn):
    """
    """
    # TODO: Currently logging does not work within a multiprocessing function.
    logging.info(f'Running proposal on page {page["page_num"]} on pdf {page["pdf_name"]}')
    bstring = page['resize_bytes']
    img = Image.open(io.BytesIO(bstring)).convert('RGB')
    coords = get_proposals(img)
    page['proposals'] = coords
    return page

#print(f"Pickle run_page: {is_picklable(run_page)}")

def propose_doc(db_insert_fn, num_processes, multimachine=False, replica_count=1, pod_id=1):
    """
    """
    logging.info('Starting proposal generation process')
    start_time = time.time()
    client = MongoClient(os.environ["DBCONNECT"])
    if multimachine:
        obj_id = str(full['_id'])
        doc_num = int(obj_id, 16)
        if doc_num % replica_count != pod_id:
            return
        logging.info('Document found and added to queue')
    db = client.pdfs
    for batch in load_docs(db, 100):
        logging.info('Loaded next batch. Running proposals')
        pages = Parallel(n_jobs=num_processes)(delayed(run_page)(page, db_insert_fn) for page in batch)
        db_insert_fn(pages, client)

    end_time = time.time()
    logging.info(f'Exiting proposal generation. Time up: {end_time - start_time}')


"""
TODO: This function no longer works with the above code. It needs to be updated to run on the page level.
"""
def propose(db_insert_fn):
    """
    On event trigger, run the proposals script
    """
    logging.info('Starting proposal generation process')
    start_time = time.time()
    client = MongoClient(os.environ["DBCONNECT"])
    logging.info(f'Connected to client: {client}. Setting watch on raw_pdfs collection')
    pod_name = os.environ["POD_NAME"]
    podre = re.search(r'\d+$', pod_name)
    pod_id = int(podre.group()) if podre else None
    if pod_id is None:
        logging.error('Error parsing for pod id')
        return
    replica_count = int(os.environ["REPLICA_COUNT"])
    db = client.pdfs
    # Open a cursor that will watch for inserts on raw_pdfs
    try:
        with db.raw_pdfs.watch([{'$match': {'operationType': 'insert'}}]) as stream:
            for doc in stream:
                propose_doc(doc, db, db_insert_fn, multimachine=True, replica_count=replica_count, pod_id=pod_id)
    except pymongo.errors.PyMongoError as err:
        logging.error("Error in pymongo:")
        logging.error(err)

    end_time = time.time()
    logging.info(f'Exiting proposal generation. Time up: {end_time - start_time}')

def mongo_insert_fn(objs, client):
    db = client.pdfs
    pages = db.propose_pages
    #result = pages.replace_one({'_id':ObjectId(record['_id'])}, obj, upsert=True)
    result = pages.insert_many(objs)
    logging.info(f"Inserted result: {result}")


@click.command()
@click.argument('num_processes')
def click_wrapper(num_processes):
    propose_doc(mongo_insert_fn, int(num_processes))


if __name__ == '__main__':
    click_wrapper()


