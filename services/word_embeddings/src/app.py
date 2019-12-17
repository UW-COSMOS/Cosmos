#
# Data service application providing access to the 'word2vec' operation and others from the Gensim model.
#

from flask import Flask, Response, request
from flask_cors import CORS, cross_origin

from gensim.models import Word2Vec
from gensim.corpora import Dictionary
from gensim.models import TfidfModel
from nltk import word_tokenize
from nltk.corpus import stopwords
stop_words = stopwords.words('english')

import json
import sys

# import logging
# import traceback


model_path = "./data/corpus_1k_model_trigram"
dictionary_path = "./data/corpus_1k_model_dictionary_trigram"
tfidf_path = "./data/corpus_1k_model_tfidf_trigram"

sys.stdout.write("Loading model from '{}'... ".format(model_path))
sys.stdout.flush()

try:
    model = Word2Vec.load(model_path)
    dct = Dictionary.load(dictionary_path)
    tfidf = TfidfModel.load(tfidf_path)

except:
    sys.stdout.write("Error: not found.\n")
    sys.exit(2)


def preprocess(line):
    line = word_tokenize(line)  # Split into words.
    line = [w.lower() for w in line]  # Lower the text.
    line = [w for w in line if not w in stop_words]  # Remove stopwords
    return line

sys.stdout.write("done.\n")

app = Flask(__name__)

if __name__ == "__main__":
    app.run()

@app.route('/')
def hello_world():
    """Data service node: this is the root node for version 1 of this data service."""
    return node_response('XDD APPLICATION PROGRAM INTERFACE v1')

@app.route('/word2vec', methods=['GET', 'POST'])
def word2vec():
    """Data service operation: execute a vector query on the specified word."""
    query_word=preprocess(request.values.get('word'))
    n_responses=int(request.values.get('n', '10'))
    if query_word:
        try:
            a = model.wv.most_similar(positive=query_word, topn=5*n_responses)
            if request.values.get('idf'):
                vals = dict(tfidf[dct.doc2bow([i[0] for i in a])])
                vals = sorted(vals.items(), key=lambda x: -x[1])
                a_idf = [(dct[i[0]], i[1]) for i in vals[:n_responses]]
            else:
                a_idf = None
            a = a[:n_responses]
        except KeyError:
            return data_response([])
        else:
            return data_response(a, data_rw=a_idf)
    else:
        return error_400("You must specify a value for the argument 'word'")

@app.errorhandler(404)
def not_found(error):
    return json_response('{ "status": "404", "error": "Page not found." }', 404)

def data_response(data, data_rw = None):
    """Return a 200 response with a JSON body containing the specified data."""
    if data_rw is None:
        resp = json_response('{ "status": "200", "data": ' + json.dumps(data) + ' }', 200)
    else:
        resp = json_response('{ "status": "200", "data": ' + json.dumps(data) + ', "data_idf_weighted" : ' + json.dumps(data_rw) + ' }', 200)
    return resp

def node_response(message):
    """Return a 200 response with a JSON body containing the specified message."""
    return json_response('{ "status": "200", "message": ' + json.dumps(message) + ' }', 200)

def error_400(message):
    """Return a 400 response with a JSON body containing the specified error message."""
    return json_response('{ "status": "400", "error": ' + json.dumps(message) + ' }', 400)

def error_500(message):
    """Return a 500 response with a JSON body indicating an internal error."""
    return json_response('{ "status": "500", "error": "A server error occurred." }', 500)

def json_response(content, status):
    r = Response(content, status, mimetype="application/json")
    r.headers["Content-Type"] = "application/json; charset=utf-8"
    return r



# return '[%s]' % ', '.join(map(str,a))
# try:
#     n = args.n

# except:
#     n = 10

# If path is to a word2vec model and words can be found.

# if(not(model_FLAG)):
# 	a = model.wv.most_similar(positive=args.word, topn=n)
# 	print('[%s]' % ', '.join(map(str,a)))
