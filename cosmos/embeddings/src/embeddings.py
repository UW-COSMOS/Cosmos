"""
Embeddings
"""
import pymongo
from pymongo import MongoClient
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import time
import click
from joblib import Parallel, delayed
import pandas as pd
import re
import pickle
from collections import defaultdict
import math


def load_pages(db, buffer_size):
    """
    """
    current_docs = []
    for doc in db.ocr_objs.find().batch_size(buffer_size):
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs


def calculate_tfidf_for_page(page, dic, dft, N):
    ocr_df = pd.DataFrame(page['page_ocr_df'])
    words = ocr_df['text']
    tf = defaultdict(int)
    strip_regx = re.compile('[^a-zA-Z]') # Strip all non alphabet characters
    for ind, word in words.iteritems():
        word = str(word)
        word = strip_regx.sub('', word)
        if not word:
            continue
        tf[word] += 1
    bow = [0] * len(dic)
    for k, v in tf.items():
        idf = math.log(N / dft[k])
        tfidf = v * idf
        bow[dic[k]] = tfidf
    page['tfidf'] = bow
    client = MongoClient(os.environ['DBCONNECT'])
    db = client.pdfs
    result = db.ocr_objs.replace_one({'_id': page['_id']}, page, False)
    return result


def calculate_tfidf(num_processes):
    logging.info('Calculating tfidf for all documents')
    start_time = time.time()
    client = MongoClient(os.environ['DBCONNECT'])
    logging.info(f'Connected to client: {client}')
    db = client.pdfs
    dic = dft = None
    if not os.path.exists('dict.p') or not os.path.exists('dft.p'):
        dic, dft = load_dict()
    else:
        dic = pickle.load(open('dict.p', 'rb'))
        dft = pickle.load(open('dft.p', 'rb'))
    num_documents = db.ocr_objs.count_documents({})
    for batch in load_pages(db, num_processes):
        results = Parallel(n_jobs=num_processes)(delayed(calculate_tfidf_for_page)(page, dic, dft, num_documents) for page in batch)
        for result in results:
            logging.info(f'Replacement result: {result}')


def load_dict():
    logging.info('Creating a dictionary from all objects in collection')
    start_time = time.time()
    client = MongoClient(os.environ['DBCONNECT'])
    logging.info(f'Connected to client: {client}')
    db = client.pdfs
    # Maintain a dictionary mapping an item to its index
    item_ind = {}
    current_ind = 0
    # maintain a document frequency mapping
    dft = defaultdict(int)
    strip_regx = re.compile('[^a-zA-Z]') # Strip all non alphabet characters
    for batch in load_pages(db, 10):
        for page in batch:
            ocr_df = pd.DataFrame(page['page_ocr_df'])
            words = ocr_df['text']
            word_list = []
            for ind, word in words.iteritems():
                word = str(word)
                word = strip_regx.sub('', word)
                if not word:
                    continue
                word_list.append(word)
                if word in item_ind:
                    continue
                item_ind[word] = current_ind
                current_ind += 1
            word_set = set(word_list)
            for word in word_set:
                dft[word] += 1

    with open('dict.p', 'wb') as wf:
        pickle.dump(item_ind, wf)
    with open('dft.p', 'wb') as wf:
        pickle.dump(dft, wf)

    return item_ind, dft


@click.command()
@click.argument('num_processes')
def click_wrapper(num_processes):
    calculate_tfidf(int(num_processes))

if __name__ == '__main__':
    click_wrapper()


