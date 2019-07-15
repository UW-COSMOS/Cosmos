"""
Some endpoints 
"""

import pymongo
from pymongo import MongoClient
from flask import Flask, request, abort
from flask.json import JSONEncoder
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from bson import json_util
import base64
import json
from flask import jsonify

class CustomJSONEncoder(JSONEncoder):
    def default(self, obj): return json_util.default(obj)


app = Flask(__name__)
#app.json_encoder = CustomJSONEncoder

@app.route('/search')
def search():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        obj_type = request.args.get('type', '')
        query = request.args.get('q', '')
        results = list(db.ocr_objs.find().limit(20))
        for result in results:
            result['_id'] = str(result['_id'])
            encoded = base64.encodebytes(result['bytes'])
            result['bytes'] = encoded.decode('ascii')
            del result['page_ocr_df']
        results_obj = {'results': results}
        return jsonify(results_obj) 
    except TypeError:
        abort(400)
    


