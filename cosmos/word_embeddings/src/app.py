#
# Data service application providing access to the 'word2vec' operation and others from the Gensim model.
#

from flask import Flask, Response, request, send_file
from flask_cors import CORS, cross_origin

from gensim.models import Word2Vec
from gensim.corpora import Dictionary
from gensim.models import TfidfModel
from nltk import word_tokenize
from nltk.corpus import stopwords
stop_words = stopwords.words('english')

from io import BytesIO, StringIO
import zipfile

import json
import sys

import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
# import traceback

def generate_zip(files):
    mem_zip = BytesIO()
    with zipfile.ZipFile(mem_zip, mode="w",compression=zipfile.ZIP_DEFLATED) as zf:
        for f in files:
            zf.writestr(f[0], f[1].getvalue())
    return mem_zip

model_path = "./data/model_100_streamed"
dictionary_path = "./data/model_100_streamed_dictionary"
tfidf_path = "./data/model_100_streamed_tfidf"

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

@app.route('/tensors', methods=['POST'])
def tensors():
    body = request.json
    terms = body['terms']
    products = get_tensors(terms)
    zipfile = generate_zip(products)
    zipfile.seek(0)
    return send_file(zipfile, attachment_filename=f"dummy.zip", as_attachment=True)

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

def get_tensors(terms):
    """
    TODO: Docstring for get_products.

    Args:
        model (TODO): TODO
        terms (TODO): TODO

    Returns: TODO

    """
    metadata = StringIO()
    tensors = StringIO()
    for word in model.wv.index2word:
#            if word == word.lower():
#                continue
        if word.lower() not in terms:
            continue
#            encoded=word.encode('utf-8')
        metadata.write(word + '\n')
        vector_row = '\t'.join(map(str, model[word]))
        tensors.write(vector_row + '\n')
    return [("tensors.tsv", tensors),
            ("metadata.tsv", metadata)]

