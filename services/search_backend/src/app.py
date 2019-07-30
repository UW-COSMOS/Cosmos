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

        s = s[:20]

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



