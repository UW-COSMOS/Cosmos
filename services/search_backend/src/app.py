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
from elasticsearch_dsl import Search, connections, Q

connections.create_connection(hosts=['es01'], timeout=20)

app = Flask(__name__)

@app.route('/search')
def search():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        obj_type = request.args.get('type', '')
        query = request.args.get('query', '')
        page_num = int(request.args.get('pageNumber', 0))
        s = Search()
        q = Q()

        if query != '':
            logging.info(query)
            q = q & Q('match', content=query)

        if obj_type != '':
            logging.info(obj_type)
            q = q & Q('match', cls=obj_type)

        s = Search().query(q)
        logging.info(s.to_dict())

        s = s[page_num*20:(page_num+1)*20]

        response = s.execute()
#        logging.info(str(response))
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

PAGE_SIZE = 1

@app.route('/search/image/next_prediction')
def pages():
    '''
    '''
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    page_num = int(request.args.get('pageNumber', 0))
    curs = db.postprocess_pages.find().sort('_id').skip(PAGE_SIZE * page_num).limit(PAGE_SIZE)
    result_list = []
    for result in curs:
        result['_id'] = str(result['_id'])
        result['pdf_id'] = str(result['pdf_id'])
        del result['bytes']
        encoded = base64.encodebytes(result['resize_bytes'])
        result['resize_bytes'] = encoded.decode('ascii')
        del result['ocr_df']
        result_list.append(result)
    results_obj = {'results': result_list}
    return jsonify(results_obj)

@app.route('/search/tags/all')
def tags():
    '''
    hardcode the tags for the time being
    '''
    resp = {"v":1,"license":"MIT","data":[{"tag_id":1,"name":"Body Text","description":"The primary text of an article","color":"#aaaaaa","created":"2019-04-02T20:04:30.849Z"},{"tag_id":2,"name":"Figure","description":"A chart, graph, or other graphical display","color":"#a15231","created":"2019-04-02T20:04:30.849Z"},{"tag_id":3,"name":"Figure Note","description":"A footnote explanation of specific content in a figure","color":"#801515","created":"2019-04-02T20:04:30.849Z"},{"tag_id":4,"name":"Figure Caption","description":"A text description associated with an entire figure","color":"#c45778","created":"2019-04-02T20:04:30.849Z"},{"tag_id":5,"name":"Table","description":"A tabular representation of information","color":"#432F75","created":"2019-04-02T20:04:30.849Z"},{"tag_id":6,"name":"Table Note","description":"A footnote to explain a subset of table content","color":"#162c57","created":"2019-04-02T20:04:30.849Z"},{"tag_id":7,"name":"Table Caption","description":"A text description associated with an entire table","color":"#73548f","created":"2019-04-02T20:04:30.849Z"},{"tag_id":8,"name":"Page Header","description":"Document-wide summary information, including page no., at top of page","color":"#2a7534","created":"2019-04-02T20:04:30.849Z"},{"tag_id":9,"name":"Page Footer","description":"Document-wide summary information, including page no., at bottom of page","color":"#345455","created":"2019-04-02T20:04:30.849Z"},{"tag_id":10,"name":"Section Header","description":"Text identifying section within text of document","color":"#1aa778","created":"2019-04-02T20:04:30.849Z"},{"tag_id":11,"name":"Equation","description":"An equation","color":"#2C4770","created":"2019-04-02T20:04:30.849Z"},{"tag_id":12,"name":"Equation label","description":"An identifier for an equation","color":"#4D658D","created":"2019-04-02T20:04:30.849Z"},{"tag_id":13,"name":"Abstract","description":"Abstract of paper","color":"#D4A26A","created":"2019-04-02T20:04:30.849Z"},{"tag_id":14,"name":"Reference text","description":"References to other works","color":"#804D15","created":"2019-04-02T20:04:30.849Z"},{"tag_id":15,"name":"Other","description":"Textual metadata and image content that is not semantically meaningful","color":"#96990c","created":"2019-04-02T20:04:30.849Z"},{"tag_id":16,"name":"Equation definition","description":"An equation definition","color":"#23477e","created":"2019-04-02T20:04:30.849Z"},{"tag_id":17,"name":"Symbol","description":"A symbol","color":"#4c2c70","created":"2019-04-02T20:04:30.849Z"},{"tag_id":18,"name":"Symbol definition","description":"A symbol definition","color":"#ff0000","created":"2019-04-02T20:04:30.849Z"}]}
    return jsonify(resp)
