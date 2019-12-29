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

db = None
num_processes =  None
index = None
word2int = None
int2word = None
xb = None

def get_index(path_to_model):
    global index
    global word2int
    global xb
    global int2word
    #model = generate_model()
    model = Word2Vec.load(path_to_model)
    word2int={key:k for k,key in enumerate(model.wv.vocab.keys())}  
    int2word ={k:key for k,key in enumerate(model.wv.vocab.keys())}
    xb=numpy.array([model.wv[word] for word in model.wv.vocab.keys()])
    quantizer = faiss.IndexFlatIP(100) # Inner product cosine similarity
    nlist = 50 # Finetune this number of clusters
    m = 100 # bytes per vector
    index = faiss.IndexIVFPQ(quantizer, 100, nlist, m, 8) # reduced accuray, fast
    #index = faiss.IndexIVFFlat(quantizer, 100, nlist, faiss.METRIC_INNER_PRODUCT)
    faiss.normalize_L2(xb)
    #print(xb.shape)
    index.train(xb)
    index.add(xb) 
    index.nprobe = 5
    # return index, word2int

def query(word, k):
    xq = numpy.array(xb[word2int[word]])
    xq = numpy.expand_dims(xq, axis=0)
    D, I = index.search(xq, k)
    return  [int2word[index] for index in I[0]]

@click.command()
@click.argument('path_to_model')
@click.argument('word')
@click.argument('k')
def click_wrapper(path_to_model, word, k):
    get_index(path_to_model)
    print(query(word, int(k)))

if __name__ == '__main__':
    click_wrapper()


