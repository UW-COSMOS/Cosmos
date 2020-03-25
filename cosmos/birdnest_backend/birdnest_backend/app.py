from flask import Flask, request, abort
from flask import jsonify
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)

app = Flask(__name__)

@app.route('/api/v1/search')
def search():
    try:
        query = request.args.get('query', type=str)
        obj_type = request.args.get('type', type=str)
        dataset_id = request.args.get('dataset_id', type=str)
        area = request.args.get('area', type=int)
        base_confidence = request.args.get('base_confidence', type=float)
        postprocessing_confidence = request.args.get('postprocessing_confidence', type=float)
        
        results_obj = {
                         "query" : query, 
                         "obj_type": obj_type, 
                         "dataset_id": dataset_id, 
                         "area": area, 
                         "base_confidence": base_confidence, 
                         "postprocessing_confidence": postprocessing_confidence
                      }
        return jsonify(results_obj)

    except TypeError as e:
        logging.info(f'{e}')
        abort(400)     

@app.route('/api/v1/get_neighborhoods')
def get_neighborhoods():
    try:
        object_id = request.args.get('object_id', type=str)
        #TODO: Body
        results_obj = {"object_id" : object_id} #dummy
        return jsonify(results_obj)
    except TypeError as e:
        logging.info(f'{e}')
        abort(400)  

@app.route('/api/v1/table/<table_id>')
def get_table(table_id):
    #TODO: Body
    results_obj = {"table_id" : table_id} #dummy
    return jsonify(results_obj)

# Testing
# @app.route('/index')
# def index():
#     return "Hello, World!"
