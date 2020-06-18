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
import os
stop_words = stopwords.words('english')

from io import BytesIO, StringIO
import zipfile

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, defer
from sqlalchemy.sql.expression import func

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
Session = sessionmaker()
Session.configure(bind=engine)

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

bibjsons = {}
models = {}
model_path = "./data/model_streamed"
bigram_model_path = "./data/model_streamed_bigram"
trigram_model_path = "./data/model_streamed_trigram"
bibjsons['default'] = "./data/bibjson"

cleaned_model_path = "./data_cleaned/model_streamed"
bigram_cleaned_model_path = "./data_cleaned/model_streamed_bigram"
trigram_cleaned_model_path = "./data_cleaned/model_streamed_trigram"
bibjsons['cleaned'] = "./data_cleaned/bibjson"

sys.stdout.write("Loading model from '{}'... ".format(model_path))
sys.stdout.flush()

try:
    models['default'] = Word2Vec.load(model_path)
    sys.stdout.write("Loaded monograms")
    sys.stdout.flush()
    models['bigram'] = Word2Vec.load(bigram_model_path)
    sys.stdout.write("Loaded bigrams")
    sys.stdout.flush()
    models['trigram'] = Word2Vec.load(trigram_model_path)
    sys.stdout.write("Loaded trigrams")
    sys.stdout.flush()

    models['default_cleaned'] = Word2Vec.load(cleaned_model_path)
    sys.stdout.write("Loaded cleaned monograms")
    sys.stdout.flush()
    models['bigram_cleaned'] = Word2Vec.load(bigram_cleaned_model_path)
    sys.stdout.write("Loaded cleaned bigrams")
    sys.stdout.flush()
    models['trigram_cleaned'] = Word2Vec.load(trigram_cleaned_model_path)
    sys.stdout.write("Loaded cleaned trigrams")
    sys.stdout.flush()

except:
    sys.stdout.write("Error: not found.\n")
    sys.exit(2)


def preprocess(line):
    line = word_tokenize(line)  # Split into words.
#    line = [w.lower() for w in line]  # Lower the text.
    line = [w for w in line if not w in stop_words]  # Remove stopwords
    return line

sys.stdout.write("done.\n")

app = Flask(__name__)
CORS(app)

if __name__ == "__main__":
    app.run()

@app.route('/')
def hello_world():
    """Data service node: this is the root node for version 1 of this data service."""
    return node_response('XDD APPLICATION PROGRAM INTERFACE v1')

@app.route('/cosmul', methods=['GET', 'POST'])
def cosmul():
    """Data service operation: execute a vector query on the specified word."""
    pos=request.values.get('positive', '').split(',')
    neg=request.values.get('negative', '').split(',')
    logging.info(f"Cosmul with pos : {pos} and negative: {neg}")

    model_name=request.values.get('model', 'default')
    cleaned=request.values.get('cleaned', 'false')
    if cleaned.lower()== "true":
        model_name += "_cleaned"
    n_responses=int(request.values.get('n', '10'))

    if pos or neg:
        try:
            a = models[model_name].wv.most_similar_cosmul(positive=pos, negative=neg, topn=n_responses)
        except KeyError:
            return data_response([])
        else:
            return data_response(a)
    else:
        return error_400("You must specify a value for the argument 'word'")

@app.route('/most_similar_to_given', methods=['GET', 'POST'])
def most_similar_to():
    entity=request.values.get('entity', '')
    entities_list=request.values.get('entities_list', '').split(',')
    model_name=request.values.get('model', 'default')
    cleaned=request.values.get('cleaned', 'false')
    if cleaned.lower()== "true":
        model_name += "_cleaned"
    if entity=='' or entities_list=='' or isinstance(entity, list):
        return error_400(f"You must specify valid entity (single term) and entities_list (comma-separated) parameters! {entity}, {entities_list}")
    try:
        a = models[model_name].wv.most_similar_to_given(entity, entities_list)
    except KeyError:
        return error_400(str(sys.exc_info()[1]))
    return data_response({'entity': entity, 'most_similar_entity': a})

@app.route('/most_similar', methods=['GET', 'POST'])
def most_similar():
    """Data service operation: execute a vector query on the specified word."""
    pos=request.values.get('positive', '').split(',')
    neg=request.values.get('negative', '').split(',')
    model_name=request.values.get('model', 'default')
    cleaned=request.values.get('cleaned', 'false')
    if cleaned.lower()== "true":
        model_name += "_cleaned"
    n_responses=int(request.values.get('n', '10'))
    logging.info(f"most_similar with positive={pos} and negative={neg}")

    if pos or neg:
        try:
            a = models[model_name].wv.most_similar(positive=pos, negative=neg, topn=n_responses)
            logging.info(model_name)
            logging.info(a)
            return data_response(a)
        except KeyError:
            return data_response([])
    else:
        return error_400("You must specify a value for the argument 'positive' or 'negative'")

@app.route('/word2vec', methods=['GET', 'POST'])
def word2vec():
    """Data service operation: execute a vector query on the specified word."""
    query_word=preprocess(request.values.get('word'))
    model_name=request.values.get('model', 'default')
    cleaned=request.values.get('cleaned', 'false')
    if cleaned.lower()== "true":
        model_name += "_cleaned"
    n_responses=int(request.values.get('n', '10'))

    if query_word:
        try:
            a = models[model_name].wv.most_similar(positive=query_word, topn=5*n_responses)
            if request.values.get('idf'):
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

@app.route('/bibjson', methods=['GET'])
    model_name=request.values.get('model', 'default')
    cleaned=request.values.get('cleaned', 'false')
    if cleaned.lower()== "true":
        model_name += "_cleaned"
    if "cleaned" in model_name:
        return send_file(bibjsons['cleaned'])
    else:
        return send_file(bibjsons['default'])

@app.route('/document_tensors/<pdf_name>', methods=['GET'])
def document_tensors(pdf_name):
    session = Session()
    res = session.execute('SELECT content FROM page_objects, pages, pdfs WHERE pdfs.id=pages.pdf_id AND pages.id=page_objects.page_id AND pdf_name=:pdf_name', {'pdf_name' : pdf_name})
    unique_words = set([])
    for obj in res:
        for word in obj.content.split():
            unique_words.add(word)
    products = get_tensors(list(unique_words))
    zipfile = generate_zip(products)
    zipfile.seek(0)
    return send_file(zipfile, attachment_filename=f"{pdf_name.replace('.pdf', 'tensors')}.zip", as_attachment=True)

@app.route('/tensors', methods=['POST'])
def tensors():
    body = request.json
    terms = body['terms']
    if "model" in body:
        model_name = body['model']
    else:
        model_name = 'default'
    cleaned=request.values.get('cleaned', 'false')
    if cleaned.lower()== "true":
        model_name += "_cleaned"
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

def bibjson():
def json_response(content, status):
    r = Response(content, status, mimetype="application/json")
    r.headers["Content-Type"] = "application/json; charset=utf-8"
    return r

def get_tensors(terms, model_name='default'):
    """
    TODO: Docstring for get_products.

    Args:
        model (TODO): TODO
        terms (TODO): TODO

    Returns: TODO

    """
    metadata = StringIO()
    tensors = StringIO()
    for word in models[model_name].wv.index2word:
#            if word == word.lower():
#                continue
        if word.lower() not in terms:
            continue
#            encoded=word.encode('utf-8')
        metadata.write(word + '\n')
        vector_row = '\t'.join(map(str, models['default'][word]))
        tensors.write(vector_row + '\n')
    return [("tensors.tsv", tensors),
            ("metadata.tsv", metadata)]

