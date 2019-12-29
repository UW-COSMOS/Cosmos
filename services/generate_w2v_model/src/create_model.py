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
from gensim.utils import simple_preprocess as tokenize
from gensim.models import Word2Vec, FastText # Create FastText model for more accuracy (but slower)

db = None
num_processes =  None

class SentencesIterator():
    def __init__(self, generator_function):
        self.generator_function = generator_function
        self.generator = self.generator_function()

    def __iter__(self):
        # reset the generator
        self.generator = self.generator_function()
        return self

    def __next__(self):
        result = next(self.generator)
        if result is None:
            raise StopIteration
        else:
            return result

def load_pages():
    """
    """
    current_docs = []
    for doc in db.objects.find().batch_size(num_processes):
        current_docs.append(tokenize(doc['content']))
        if len(current_docs) == num_processes:
            yield [i for doc in current_docs for i in doc]
            current_docs = []
    yield [i for doc in current_docs for i in doc]

#Create W2V model and save to specified path
def generate_model(path_to_model):
    global db
    logging.info('Generating gensim model for all documents')
    start_time = time.time()
    client = MongoClient(os.environ['DBCONNECT'])
    logging.info(f'Connected to client: {client}')
    db = client.pdfs
    sentences = SentencesIterator(load_pages)
    print('Calculating the embeddings...')
    model = Word2Vec(sentences, size=100, window=10, min_count=3, workers=4)
    print('Saving the model...')
    model.save(path_to_model)
    print('WORD2VEC Model saved.')
    return model

@click.command()
@click.argument('n_processes')
@click.argument('path_to_model')
def click_wrapper(n_processes, path_to_model):
    global num_processes
    num_processes = int(n_processes)
    generate_model(path_to_model)

if __name__ == '__main__':
    click_wrapper()


