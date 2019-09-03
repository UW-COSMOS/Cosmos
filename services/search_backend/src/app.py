"""
Some endpoints 
"""

import pickle
import pymongo
from pymongo import MongoClient
from bson.objectid import ObjectId
from flask import Flask, request, abort
from flask.json import JSONEncoder
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from bson import json_util
import base64
import json
from flask import jsonify
from elasticsearch import Elasticsearch
from elasticsearch_dsl import Search, connections
import re
import spacy
import requests
import pandas as pd
from .values_query import values_query

connections.create_connection(hosts=['es01'], timeout=20)

app = Flask(__name__)

nlp = spacy.load('en_core_web_lg')

@app.route('/analyze', methods=['POST'])
def analyze():
    try:
        file_content = request.get_json().get('file', '')
    except Exception as e:
        logging.info(f'{e}')

    comment_reg = re.compile('.*(::|REAL|real|logical|=|LOGICAL|integer|INTEGER).*!(.*)')
    loc = []
    for i, line in enumerate(file_content.split('\n')):
        m = comment_reg.search(line)
        if m is not None:
            comment = m.group(2)
            doc = nlp(comment)
            chunks = [chunk.text for chunk in doc.noun_chunks]
            for chunk in chunks:
                loc.append({'phrase': chunk, 'line': line, 'line_number': i})
    final_obj = {'results': loc}
    return jsonify(final_obj) 


def postprocess_result(result):
    result['_id'] = str(result['_id'])
    if 'bytes' in result and result['bytes'] is not None:
        encoded = base64.encodebytes(result['bytes'])
        result['bytes'] = encoded.decode('ascii')
        del result['page_ocr_df']
    if 'table_df' in result and result['table_df'] is not None:
        encoded = base64.encodebytes(result['table_df'])
        result['table_df'] = encoded.decode('ascii')
    return result

@app.route('/search')
def search():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        obj_type = request.args.get('type', '')
        query = request.args.get('q', '')
        s = Search().query('match', content=query).query('match', cls=obj_type)[:20]#.filter('term', cls=obj_type)[:20]
        response = s.execute()
        logging.info(str(response))
        result_list = []
        for result in response:
            id = result.meta.id
            obj_id = ObjectId(id)
            res = None
            if result['cls'] == 'code':
                res = db.code_objs.find_one({'_id': obj_id})
            else:
                res = db.ocr_objs.find_one({'_id': obj_id})
            result_list.append(res)

        result_list = [postprocess_result(r) for r in result_list]

        results_obj = {'results': result_list}
        return jsonify(results_obj) 
    except TypeError as e:
        logging.info(f'{e}')
        abort(400)


@app.route('/values')
def values():
    client = MongoClient(os.environ['DBCONNECT'])
    db = client.pdfs
    try:
        query = request.args.get('q', '')
        values, oids = values_query(query)
        result_list = []
        for oid in oids:
            r = db.objects.find_one({'_id': oid})
            result_list.append(r)

        result_list = [postprocess_result(r) for r in result_list]
        return jsonify({'results': result_list, 'values': values})
    except TypeError:
        abort(400)


qa_URL = 'http://qa:4000/query'
@app.route('/qa1')
def qa1():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        obj_type = 'Body Text'
        query = request.args.get('q', '')
        s = Search().query('match', content=query).query('match', cls=obj_type)[:20]#.filter('term', cls=obj_type)[:20]
        response = s.execute()
        logging.info(str(response))
        result_list = []
        
        for result in response:
            id = result.meta.id
            obj_id = ObjectId(id)
            if result['cls'] == 'code':
                res = db.code_objs.find_one({'_id': obj_id})
            else:
                res = db.ocr_objs.find_one({'_id': obj_id})
            candidate = res['content']
            logging.debug(candidate)
            answer_l = list(requests.get(qa_URL, {'query':query, 'candidate':candidate}).json().values())
            logging.debug("qa answer ready")
            if len(answer_l) > 0:
                res['answer'] = str(answer_l[0])
            result_list.append(res)
            break

        for result in result_list:
            result['_id'] = str(result['_id'])
            if 'bytes' in result:
                encoded = base64.encodebytes(result['bytes'])
                result['bytes'] = encoded.decode('ascii')
                del result['page_ocr_df']
        results_obj = {'results': result_list}
        return jsonify(results_obj) 
    except TypeError:
        abort(400)


@app.route('/qa')
def qa():
    try:
        query = request.args.get('q', '')
        query = query.lower()
        if query == 'toc' or query == 'total organic carbon':
            return jsonify({'definition': 'Total organic carbon (TOC) is the amount of carbon found in an organic compound and is often used as a non-specific indicator of water quality or cleanliness of pharmaceutical manufacturing equipment. TOC may also refer to the amount of organic carbon in soil, or in a geological formation, particularly the source rock for a petroleum play; 2% is a rough minimum.[1] For marine surface sediments, average TOC content is 0.5% in the deep ocean, and 2% along the eastern margins.'})
    except TypeError:
        abort(400)

@app.route('/data')
def data():
    try:
        query = request.args.get('q', '')
        query = query.lower()
        if query == 'toc' or query == 'total organic carbon':
            return_obj = {'data': [
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    },
                                    {
                                        'doi': 'https://doi.org/10.1109/5.771073', 
                                        'value': '10'
                                    }]}
            return jsonify(return_obj)
    except TypeError:
        abort(400)


    


