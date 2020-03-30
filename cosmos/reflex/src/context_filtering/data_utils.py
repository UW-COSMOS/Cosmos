import numpy
import json
import sqlite3
import random
import spacy
from collections import Counter
from multiprocessing import Pool

filter_tokens = ['.', ',', '(', ')', '</s>', '_._', ':', '-', ',', '_..._', '_:_']

def chunk_data(data, n):
    data_len = len(data)
    chunk_size = data_len//n
    print("chunk_size : ", chunk_size)
    return [data[i:i+chunk_size] for i in range(0, data_len, chunk_size)]

def process_rows(rows):
    word_count = Counter()
    for row in rows:
        doc = nlp(row[1].strip())
        for token in doc:
            word = token.text
            #if word in filter_tokens:
            #    continue
            word_count[word] += 1
    
    return word_count

def compute_word_freq(db_dump, out_file, nlp):
    '''
    Computes frequency and writes to a json file.
    :param db_dump - path to db file containing docs
    :param out_file - the word freqency is written to this file
    :param nlp - spacy model
    '''

    print("Connecting to db ...")
    conn = sqlite3.connect(db_dump)
    cursor = conn.cursor()
    cursor.execute("SELECT * FROM documents")
    rows = cursor.fetchall()

    num_processes = 60
    row_chunks = chunk_data(rows, num_processes)

    print("Processing data ...")
    pool = Pool(num_processes)
    results = pool.map(process_rows, row_chunks)
    word_count = Counter()
    for result in results:
        word_count += result 
    '''
    # Single process code
    word_count = Counter()
    for row in rows:
        doc = nlp(row[1].strip())
        for token in doc:
            word = token.text
            #if word in filter_tokens:
            #    continue
            word_count[word] += 1
    '''
    with open(out_file, 'w') as ofile:
        json.dump(word_count, ofile)
    
def compute_word_weight(freq_file, word_weight_file, a=1e-3):
    '''
    Computes word weight
    :param freq_file - contains word freuency
    :param word_weight_file - word weight written to this file.
    :param a - smoothing factor
    '''
    if a <=0: # when the parameter makes no sense, use unweighted
        a = 1.0
    with open(freq_file, 'r') as f1:
        word_freq = json.load(f1)
        total = sum(word_freq.values())

        with open(word_weight_file, 'w') as f2:
            word_weight = Counter()
            for word in word_freq:
                word_weight[word] = a / (a + float(word_freq[word])/total)
            json.dump(word_weight, f2)

if __name__ == '__main__':

    db_dump = 'data/contexts.db'
    freq_file = 'data/wiki_word_freq.json'
    word_weight_file = 'data/wiki_word_weight.json'

    nlp = spacy.load('en_core_web_lg')

    #compute_word_freq(db_dump, freq_file, nlp)
    compute_word_weight(freq_file, word_weight_file)
 
