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

class CustomJSONEncoder(JSONEncoder):
    def default(self, obj): return json_util.default(obj)


app = Flask(__name__)
app.json_encoder = CustomJSONEncoder

@app.route('/search')
def search():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        obj_type = request.args.get('type', '')
        query = request.args.get('q', '')
        results = list(db.ocr_objs.find().limit(20))
        results_obj = {'results': results}
        return results_obj
    except TypeError:
        abort(400)
    


