"""
Some endpoints 
"""

import pymongo
from pymongo import MongoClient
from bson.objectid import ObjectId
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

connections.create_connection(hosts=['es01'], timeout=20)

from celery import Celery
import logging
from celery.utils.log import get_task_logger

from qa_model.infer import infer_qa

CELERY_BROKER_URL = "amqp://admin:mypass@rabbit:5672"
CELERY_RESULT_BACKEND = "redis://redis:6380/0"

def qa(object_type, query):
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
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
        res['answer'] = infer_qa(query, res["raw_text"])
        result_list.append(res)
    for result in result_list:
        result['_id'] = str(result['_id'])
        if 'bytes' in result:
            encoded = base64.encodebytes(result['bytes'])
            result['bytes'] = encoded.decode('ascii')
            del result['page_ocr_df']
    results_obj = {'results': result_list}
    return jsonify(results_obj) 

