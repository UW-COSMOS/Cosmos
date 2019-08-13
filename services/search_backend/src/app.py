"""
Some endpoints 
"""

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
import requests
connections.create_connection(hosts=['es01'], timeout=20)

app = Flask(__name__)

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
            res = None
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


    


