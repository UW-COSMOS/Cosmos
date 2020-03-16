"""
"""

import pickle
import yaml
from flask.json import JSONEncoder
from flask import Flask, request, abort
from flask import jsonify, send_file
import os
from tabulate import tabulate
from schema import Pdf, Page, PageObject
from ast import literal_eval as make_tuple
from io import BytesIO
import logging
import random
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import base64
import html
import json
import re
import requests
import pandas as pd
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, defer
from sqlalchemy.sql.expression import func

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
Session = sessionmaker()
Session.configure(bind=engine)

app = Flask(__name__)

def postprocess_result(result):
    if "_id" in result:
        result['_id'] = str(result['_id'])
    if 'bytes' in result and result['bytes'] is not None:
        encoded = base64.encodebytes(result['bytes'])
        result['bytes'] = encoded.decode('ascii')
    if 'page_ocr_df' in result:
        del result['page_ocr_df']
    if 'table_df' in result and result['table_df'] is not None:
        encoded = base64.encodebytes(result['table_df'])
        result['table_df_str'] = pickle.loads(result['table_df']).to_string()
        result['table_df_csv'] = pickle.loads(result['table_df']).to_csv()
        result['table_df_tabulate'] = tabulate(pickle.loads(result['table_df']), headers='keys', tablefmt='psql')
        result['table_df'] = encoded.decode('ascii')
    return result

@app.route('/search/preview')
def download():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        _id = request.args.get('id', '')
        obj_id = ObjectId(_id)
        res = db.objects.find_one({'_id': obj_id})
        table = tabulate(pickle.loads(res['table_df']), tablefmt='psql')
        table = html.escape(table)
        return f"<div><pre>{table}</pre></div>"

    except TypeError as e:
        logging.info(f'{e}')
        abort(400)

@app.route('/search/get_dataframe')
def get_dataframe():
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    try:
        _id = request.args.get('id', '')
        obj_id = ObjectId(_id)
        res = db.objects.find_one({'_id': obj_id})
        buffer = BytesIO()
        buffer.write(res["table_df"])
        buffer.seek(0)
        return send_file(buffer, as_attachment=True, attachment_filename=f"{_id}.pickle")
    except TypeError as e:
        logging.info(f'{e}')
        abort(400)

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

@app.route('/search/image/<page_id>')
@app.route('/search/page/<page_id>')
def page_by_id(page_id):
    '''
    '''
    session = Session()
    if page_id == "next_prediction":
        # Hacky way to get a random, because sorting by rand().limit(1) is real slow
        rowCount = int(session.query(Page).count())
        rand = random.randrange(0, rowCount)
        page = session.query(Page).filter(Page.id==int(rand)).first()
    else:
        page, pdf = session.query(Page, Pdf).filter(Page.id == page_id).first()
    # temp hack -- bringing back the full model sucks for large docs because of the metadata field, so just bring back the column we care about IAR - 10.Mar.2020

    res = session.execute('SELECT pdf_name FROM pdfs WHERE id =:pdf_id LIMIT 1', {'pdf_id' : page.pdf_id})
    for r in res:
        pdf_name = r['pdf_name']
    logging.info(pdf_name)
    result = {}
    result["_id"] = page.id
    result['pdf_id'] = page.pdf_id
    result['pdf_name'] = pdf_name
    result['page_num'] = page.page_number
    result['page_width'] = page.page_width
    result['page_height'] = page.page_height
    encoded = base64.encodebytes(page.bytes)
    result['resize_bytes'] = encoded.decode('ascii')
    result['pp_detected_objs'] = []

    res = session.query(PageObject).filter(page.id == PageObject.page_id)
    for po in res.all():
        result['pp_detected_objs'].append({"bounding_box" : [float(i) for i in po.bounding_box], "class" : po.cls, "confidence" : str(po.confidence), "obj_id" : po.id})

    results_obj = {'results': [result]}
    return jsonify({"results" : [result]})

@app.route('/search/object/<obj_id>')
def object_lookup(obj_id):
    '''
    Look up object by pdf_name, page_num, and bbox
    # TODO: could extend this to just accept a point if I get clever with the mongo query.

    page_num
    pdf_name
    "bounding_box" : [
        x1,
        y1,
        x2,
        y2
        ]

    '''
    session = Session()
    if obj_id == "lookup":
        pdf_name = request.args.get("pdf_name")
        page_num = int(request.args.get('page_num'))
        x, y = make_tuple(request.args.get("coords"))
        tobj = find_object(pdf_name, page_num, x, y)
    else:
        tobj = session.query(PageObject).get(obj_id)
    obj =  {c.name: getattr(tobj, c.name) for c in tobj.__table__.columns}
    return jsonify({"results" : [postprocess_result(obj)]})

#@app.route('/search/object/lookup')
#def object_lookup():
#    '''
#    Look up object by pdf_name, page_num, and bbox
#    # TODO: could extend this to just accept a point if I get clever with the mongo query.
#
#    page_num
#    pdf_name
#    "bounding_box" : [
#        x1,
#        y1,
#        x2,
#        y2
#        ]
#
#    '''
#    session = Session()
#    pdf_name = request.args.get("pdf_name")
#    page_num = int(request.args.get('page_num'))
#    x, y = make_tuple(request.args.get("coords"))
#    tobj = find_object(pdf_name, page_num, x, y)
#    obj =  {c.name: getattr(tobj, c.name) for c in tobj.__table__.columns}
#    return jsonify({"results" : [postprocess_result(obj)]})

def find_object(pdf_name, page_num, x, y):
    session = Session()
    logging.info(f"Looking for page {page_num} from pdf {pdf_name}")
    res = session.query(Page, PageObject, Pdf).\
            filter(Pdf.pdf_name == pdf_name).\
            filter(Page.page_number == page_num).\
            filter(Pdf.id == Page.pdf_id).\
            filter(Page.id == PageObject.page_id)

    obj = {}
    for p, po, pdf in res:
        bbox = po.bounding_box
        if x >= bbox[0] and y >= bbox[1] and x <= bbox[2] and y <= bbox[3]:
            obj = po
    if obj == {}:
        logging.warning(f"Couldn't find object with coords ({x}, {y})")
    return obj

# TODO: probably shouldn't hardcode these.
@app.route('/search/tags/all')
def tags():
    '''
    hardcode the tags for the time being
    '''
    resp = {"v":1,"license":"MIT","data":[{"tag_id":1,"name":"Body Text","description":"The primary text of an article","color":"#aaaaaa","created":"2019-04-02T20:04:30.849Z"},{"tag_id":2,"name":"Figure","description":"A chart, graph, or other graphical display","color":"#a15231","created":"2019-04-02T20:04:30.849Z"},{"tag_id":3,"name":"Figure Note","description":"A footnote explanation of specific content in a figure","color":"#801515","created":"2019-04-02T20:04:30.849Z"},{"tag_id":4,"name":"Figure Caption","description":"A text description associated with an entire figure","color":"#c45778","created":"2019-04-02T20:04:30.849Z"},{"tag_id":5,"name":"Table","description":"A tabular representation of information","color":"#432F75","created":"2019-04-02T20:04:30.849Z"},{"tag_id":6,"name":"Table Note","description":"A footnote to explain a subset of table content","color":"#162c57","created":"2019-04-02T20:04:30.849Z"},{"tag_id":7,"name":"Table Caption","description":"A text description associated with an entire table","color":"#73548f","created":"2019-04-02T20:04:30.849Z"},{"tag_id":8,"name":"Page Header","description":"Document-wide summary information, including page no., at top of page","color":"#2a7534","created":"2019-04-02T20:04:30.849Z"},{"tag_id":9,"name":"Page Footer","description":"Document-wide summary information, including page no., at bottom of page","color":"#345455","created":"2019-04-02T20:04:30.849Z"},{"tag_id":10,"name":"Section Header","description":"Text identifying section within text of document","color":"#1aa778","created":"2019-04-02T20:04:30.849Z"},{"tag_id":11,"name":"Equation","description":"An equation","color":"#2C4770","created":"2019-04-02T20:04:30.849Z"},{"tag_id":12,"name":"Equation label","description":"An identifier for an equation","color":"#4D658D","created":"2019-04-02T20:04:30.849Z"},{"tag_id":13,"name":"Abstract","description":"Abstract of paper","color":"#D4A26A","created":"2019-04-02T20:04:30.849Z"},{"tag_id":14,"name":"Reference text","description":"References to other works","color":"#804D15","created":"2019-04-02T20:04:30.849Z"},{"tag_id":15,"name":"Other","description":"Textual metadata and image content that is not semantically meaningful","color":"#96990c","created":"2019-04-02T20:04:30.849Z"},{"tag_id":16,"name":"Equation definition","description":"An equation definition","color":"#23477e","created":"2019-04-02T20:04:30.849Z"},{"tag_id":17,"name":"Symbol","description":"A symbol","color":"#4c2c70","created":"2019-04-02T20:04:30.849Z"},{"tag_id":18,"name":"Symbol definition","description":"A symbol definition","color":"#ff0000","created":"2019-04-02T20:04:30.849Z"}]}
    return jsonify(resp)

@app.route('/search/page/<xdd_docid>/<page_num>')
def page(xdd_docid, page_num):
    '''
    '''
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    curs = db.propose_pages.find({"pdf_name": f"{xdd_docid}.pdf", "page_num": int(page_num)})
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

@app.route('/search/objects/<xdd_docid>/<page_num>')
def objects(xdd_docid, page_num):
    '''
    '''
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    curs = db.objects.find({"pdf_name": f"{xdd_docid}.pdf", "page_num": int(page_num)})
    result_list = []
    for result in curs:
        result_list.append(postprocess_result(result))
    results_obj = {'results': result_list}
    return jsonify(results_obj)


with open('annotations_allowed.yml') as f:
    ANNOTATIONS_ALLOWED = yaml.load(f, Loader=yaml.FullLoader)

@app.route('/search/object/annotate', methods=['POST', 'GET'])
def object_annotate():
    '''
    '''
    session = Session()
    if request.method == "GET":
        return jsonify(ANNOTATIONS_ALLOWED)

    if request.method == "POST":
        data = request.get_json(force=True)
        object_id = data.get("object_id", None)
        pdf_name = data.get("pdf_name", None)
        page_num = int(data.get('page_num', -1))
        try:
            print(data.get("coords"))
            x, y = make_tuple(data.get("coords"))
        except Exception as err:
            print(err)
            print(sys.exc_info())
            x = None
            y = None

        if object_id is None and (x is None or y is None or pdf_name is None or page_num==-1):
            abort(400)

        # get objectid from coords
        if object_id is None:
            object_id = find_object(pdf_name, page_num, x, y).id

        success = False
        for k, v in data.items():
            if k not in ANNOTATIONS_ALLOWED.keys(): continue

            atype = ANNOTATIONS_ALLOWED[k]
            if atype == "text" :
                pass
            elif atype == "boolean":
                if str(v).lower() == "true" :
                    v = True
                elif str(v).lower() == "false" :
                    v = False

            try:
                session.query(PageObject).filter(PageObject.id == object_id).update({k: v})
                session.commit()
                success=True
            except:
                logging.warning(f"Could not update object {object_id} with {k} : {v}!")
        return json.dumps({'success':success}), 200, {'ContentType':'application/json'}
