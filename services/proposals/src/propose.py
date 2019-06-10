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
from preprocess import resize_png
from connected_components import get_proposals
from typing import Mapping, TypeVar, Callable
import re

T = TypeVar('T')

def propose(db_insert_fn: Callable[[Mapping[T, T], T], None]) -> None:
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
                full = doc['fullDocument']
                obj_id = str(full['_id'])
                doc_num = int(obj_id, 16)
                if doc_num % replica_count != pod_id:
                    continue
                logging.info('Document found and added to queue')
                page_data = full['page_data']
                for page in page_data:
                    bstring = page['bytes']
                    bytesio = io.BytesIO(bstring)
                    img = resize_png(bytesio)
                    # Convert it back to bytes
                    resize_bytes_stream = io.BytesIO()
                    img.save(resize_bytes_stream, format='PNG')
                    resize_bytes = resize_bytes_stream.getvalue()
                    coords = get_proposals(img)
                    page['resize_bytes'] = resize_bytes
                    page['proposals'] = coords
                db_insert_fn(full, client)
    except pymongo.errors.PyMongoError as err:
        logging.error("Error in pymongo:")
        logging.error(err)
            
    end_time = time.time()
    logging.info(f'Exiting proposal generation. Time up: {end_time - start_time}')

def mongo_insert_fn(obj: Mapping[T, T], client: T) -> None:
    db = client.pdfs
    cc_pdfs = db.cc_pdfs
    result = cc_pdfs.insert_one(obj)
    logging.info(f"Inserted result: {result}")


if __name__ == '__main__':
    propose(mongo_insert_fn)


