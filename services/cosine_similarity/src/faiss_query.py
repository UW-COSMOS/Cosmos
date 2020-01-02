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
from gensim.models import Word2Vec, FastText # Create FastText model for more accuracy (but slower)
import numpy
import faiss

class index():
    def __init__(self, path_to_model):
        model = Word2Vec.load(path_to_model)
        self.word2int={key:k for k,key in enumerate(model.wv.vocab.keys())}
        self.int2word ={k:key for k,key in enumerate(model.wv.vocab.keys())}
        self.xb=numpy.array([model.wv[word] for word in model.wv.vocab.keys()])

    def train(self):
        quantizer = faiss.IndexFlatIP(100) # Inner product cosine similarity
        nlist = 50 # Finetune this number of clusters
        m = 100 # bytes per vector
        index = faiss.IndexIVFPQ(quantizer, 100, nlist, m, 8) # reduced accuray, fast
        #index = faiss.IndexIVFFlat(quantizer, 100, nlist, faiss.METRIC_INNER_PRODUCT)
        faiss.normalize_L2(self.xb)
        logging.info("Created faiss index")
        index.train(self.xb)
        index.add(self.xb)
        index.nprobe = 5 # number of lists to search (set to nlist for exhuastive search)
        faiss.write_index(index, "index.faiss")
        logging.info("Trained and saved faiss index")

    def query(self, word, k):
        xq = numpy.array(self.xb[self.word2int[word]])
        xq = numpy.expand_dims(xq, axis=0)
        D, I = faiss.read_index("index.faiss").search(xq, k)
        return  [self.int2word[index] for index in I[0]]

@click.command()
@click.argument('retrain')
@click.argument('path_to_model')
@click.argument('word')
@click.argument('k')
def click_wrapper(retrain, path_to_model, word, k):
    ind = index(path_to_model)
    if retrain == 'True':
        ind.train()
    print(ind.query(word, int(k)))

if __name__ == '__main__':
    click_wrapper()

