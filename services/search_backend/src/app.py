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
from elasticsearch_dsl import Search, connections, Q
import re
import spacy
import requests
import pandas as pd
from values_query import values_query

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
        _id = request.args.get('id', '')
        obj_type = request.args.get('type', '')
        query = request.args.get('query', '')
        page_num = int(request.args.get('pageNumber', 0))
        offset = int(request.args.get('offset', 0))
        logging.info(f"offset is {offset} and page_num is {page_num}")
        if offset != 0 and page_num == 0:
            page_num = offset/20
        logging.info(f"and now offset is {offset} and page_num is {page_num}")
        s = Search()
        q = Q()

        result_list = []
        if _id == '':
            logging.info("no id specified")
            if query != '':
                logging.info(f"querying for content: {query}")
                q = q & Q('match', content=query)

            if obj_type != '':
                if not obj_type.endswith("Context"):
                    obj_type += "Context"
                logging.info(f"querying for cls: {obj_type}")
                q = q & Q('match', cls=obj_type)

            if "biomass_filter" in request.args:
                logging.info("Biomass filter specified -- adding additional keyword filters")
                q = q & Q('bool', should=[
                    Q('match', content='abundance'),
                    Q('match', content='distribution'),
                    Q('match', content='biomass'),
                    Q('match', content='mass'),
                    Q('match', content='density'),
                    ])

            s = Search().query(q)
    #        logging.info(s.to_dict())

            logging.info(f"Getting results {page_num*20} to {(page_num+1)*20}")
            s = s[page_num*20:(page_num+1)*20]

            response = s.execute()
    #        logging.info(str(response))
            content_set = set()
            for result in response:
    #            logging.info(result)
                id = result.meta.id
                obj_id = ObjectId(id)
    #            logging.info(obj_id)
                res = None
                if result['cls'] == 'code':
                    res = db.code_objs.find_one({'_id': obj_id})
                elif result['cls'] == 'Section':
                    sc = db.sections.find_one({'_id': obj_id})
                    if len(sc['objects']) == 0:
                        continue
                    bt = sc['objects'][0]['_id']
                    res = db.objects.find_one({'_id': bt})
                elif result['cls'] == 'FigureContext':
                    fc = db.figureContexts.find_one({'_id': obj_id})
                    figure = fc['figure']['_id']
                    res = db.objects.find_one({'_id': figure})
                elif result['cls'] == 'EquationContext':
                    ec = db.equationContexts.find_one({'_id': obj_id})
                    eq = ec['equation']['_id']
                    res = db.objects.find_one({'_id': eq})
                elif result['cls'] == 'TableContext':
                    tc = db.tableContexts.find_one({'_id': obj_id})
                    table = tc['table']['_id']
                    res = db.objects.find_one({'_id': table})
                else:
                    res = db.objects.find_one({'_id': obj_id})
                if res['content'] in content_set:
                    continue
                content_set.add(res['content'])
                result_list.append(res)
        else:  # passed in a specific object id
            logging.info("no id specified, skipping ES junk")
            obj_id = ObjectId(_id)
            res = db.objects.find_one({'_id': obj_id})
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
        print(values_query)
        values, oids = values_query(query)
        result_list = []
        for oid in oids:
            r = db.objects.find_one({'_id': oid})
            result_list.append(r)

        result_list = [postprocess_result(r) for r in result_list]
        return jsonify({'results': result_list, 'values': values})
    except Exception as e:
        logging.error(e)
        abort(400)


threshold_qa = 0.5
qa_URL = 'http://qa:4000/query'
@app.route('/qa')
def qa():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        query = request.args.get('q', '')
        s = Search().query('match', content=query).query('match', cls='Section')[:25]
        response = s.execute()
#        logging.info(response)
        result_list = []
        content_set = set()
        for obj in response:
            id = obj.meta.id
            obj_id = ObjectId(id)

            res = None
            logging.info(id)
            res = db.sections.find_one({'_id': obj_id})
            if res is not None:
                if res['content'] in content_set:
                    continue
                content_set.add(res['content'])
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
