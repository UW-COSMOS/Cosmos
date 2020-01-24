"""
Page API endpoints
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

app = Flask(__name__)

PAGE_SIZE = 200

@app.route('/api/v1/pages')
def pages():
    '''
    200 Pages per pagination iteration, pass page #
    '''
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    page_num = int(request.args.get('pageNumber', ''))
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


    

    
