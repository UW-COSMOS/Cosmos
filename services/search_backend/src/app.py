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
        s = Search().query('match', content=query)#.query('match', cls=obj_type)[:20]#.filter('term', cls=obj_type)[:20]
        response = s.execute()
        logging.info(str(response))
        result_list = []
        for result in response:
            logging.info(result)
            id = result.meta.id
            obj_id = ObjectId(id)
            logging.info(obj_id)
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


threshold_qa = 0.85
qa_URL = 'http://qa:4000/query'
@app.route('/qa')
def qa():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        query = request.args.get('q', '')
        s = Search().query('match', content=query)[:25]
        response = s.execute()
        logging.info(response)
        result_list = []
        for obj in response:
            id = obj.meta.id
            obj_id = ObjectId(id)

            res = None
            logging.info(id)
            res = db.sections.find_one({'_id': obj_id})
            if res is not None:
                logging.info(res['pdf_name'])
                candidate = res['content']
                answer = requests.get(qa_URL, {'query':query, 'candidate':candidate}).json()
                logging.info(answer)
                result = {}
                if len(answer) > 0 and answer['probability'] > threshold_qa:
                    result['answer'] = str(answer['answer'])
                    result['probability'] = answer['probability']
                    result['content'] = res['content']
                    result['pdf_name'] = res['pdf_name']
                    result_list.append(result)
        result_list = sorted(result_list, key = lambda i : i['probability'], reverse=True)
        logging.info(result_list)
        results_obj = {'results': result_list}
        return jsonify(results_obj) 
    except TypeError as e:
        logging.info(f'{e}')
        abort(400)
    except Exception as e:
        logging.info(e)

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


    


