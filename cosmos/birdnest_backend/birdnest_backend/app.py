from flask import Flask, request, abort
from flask import jsonify
import logging
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, defer
from sqlalchemy.sql.expression import func
from sqlalchemy.sql import text
import os
import json
import base64
from retrieval.retrieve import Retrieval
from collections import defaultdict
from elasticsearch_dsl import Search, connections, Q
from schema import Pdf, Page, PageObject, Table
from flask_cors import CORS
import requests
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)

app = Flask(__name__)
CORS(app)
dataset_id = os.environ['DATASET_ID']

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True, pool_size=50, max_overflow=0)
Session = sessionmaker()
Session.configure(bind=engine)

connections.create_connection(hosts=['es01'], timeout=20)

retrievers = {}

for ctype in ["Section", "FigureContext", "TableContext", "EquationContext", "Combined"]:
    doc_idx_path = os.path.join('/index_dir/', dataset_id, ctype, 'documents_jsonl/lucene-index')
    logging.info('/index_dir/' + dataset_id +  ctype + 'documents_jsonl/lucene-index')
    logging.info(os.path.join('/index_dir/', dataset_id, ctype, 'documents_jsonl/lucene-index'))
    context_idx_pth = os.path.join('/index_dir/', dataset_id, ctype, 'contexts_jsonl/lucene-index')
    doc2odoc_pth = os.path.join('/index_dir/', dataset_id, ctype, 'id_to_pdf.pkl')
    ctx2octx_pth = os.path.join('/index_dir/', dataset_id, ctype, 'id_to_context.pkl')
    octx2odoc_pth = os.path.join('/index_dir/', dataset_id, ctype, 'context_to_doc.pkl')
    retrievers[ctype] = Retrieval(doc_idx_path, context_idx_pth, doc2odoc_pth, ctx2octx_pth, octx2odoc_pth, k1=5000, k2=1000)

objtype_index_map = {
        "Figure" : "FigureContext",
        "Equation" : "EquationContext",
        "Table" : "TableContext",
        "Body Text" : "Section",
        }

def get_bibjson(pdf_name):
    xdd_docid = pdf_name.replace(".pdf", "")
    logging.info(f"Getting bibjson for {xdd_docid}")
    resp = requests.get(f"https://geodeepdive.org/api/articles?docid={xdd_docid}")
    if resp.status_code == 200:
        data = resp.json()
        bibjson = data["success"]["data"][0]
    else:
        bibjson = {"Error" : "Could not retrieve article data"}
    return bibjson

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

query_map = {} # keep track of result page_number -> index of last object on that page from anserini context list
@app.route('/api/v1/count', endpoint='count')
@app.route('/api/v1/search', endpoint='search')
def search():
    conn = engine.connect()
    try:
        query = request.args.get('query', type=str)
        obj_type = request.args.get('type', type=str)
        area = request.args.get('area', type=int)
        page_num = request.args.get('page', type=int)
        ignore_bytes = request.args.get('ignore_bytes', type=bool)
        logging.info(f"Ignore bytes: {ignore_bytes}")
        if page_num is None:
            page_num = 0
        base_confidence = request.args.get('base_confidence', type=float)
        postprocessing_confidence = request.args.get('postprocessing_confidence', type=float)
        logging.info(f"Using {objtype_index_map.get(obj_type, 'Combined')} retriever")
        retriever = retrievers[objtype_index_map.get(obj_type, "Combined")]

        # if we have a page_number, look up what object_page_number we need
        query_name = f"{query}_{obj_type}_{area}_{base_confidence}_{postprocessing_confidence}"
        subselect = False
        if query_name not in query_map:
            query_map[query_name] = {}
        if page_num-1 in query_map[query_name]:
            start_ind = query_map[query_name][page_num-1] + 1
        else:
            # TODO: If not, we'll have to do some logic, if it's not the first page
            logging.info(f"Couldn't find query ({page_num}, {query_name}). Starting over.")
            logging.info(query_map)
            if page_num != 0:
                subselect = True
            start_ind = 0

        if request.endpoint == 'count':
            subselect = True

        logging.info(f"Searching starting at {start_ind}")
        results = retriever.search(query, start_index=start_ind) # ([actual_doc_id, actual_context_id, content, score, i])
        obj_ids = [r[1] for r in results]
        context_index = [r[4] for r in results]
        logging.info(f"{len(obj_ids)} matching contexts found!")
        if len(obj_ids) == 0:
            return {'page': 0, 'objects': []}
        objects = []

        for ind, obj in enumerate(obj_ids):
            header_q = text("select page_objects.id, page_objects.bytes, page_objects.content, page_objects.cls, JSON_EXTRACT(page_objects.init_cls_confidences, '$[0][0]'), page_objects.confidence, pages.page_number, pdfs.pdf_name from pages, page_objects, object_contexts as oc, pdfs where oc.id = :oid and page_objects.id=oc.header_id and page_objects.page_id=pages.id and pdfs.id=oc.pdf_id;")
            res1 = conn.execute(header_q, oid=obj)
            pdf_name = None
            context_id = obj
            header = {
                    'id': None,
                    'bytes' : None,
                    'content' : None,
                    'page_number' : None,
                    'cls': None,
                    'postprocessing_confidence' : None,
                    'base_confidence' : None
                    }
            for id, bytes, content, cls, h_base_confidence, h_postprocessing_confidence, page_number, pdf_name in res1:
                header['id'] = id
                header['bytes'] = base64.b64encode(bytes).decode('utf-8')
                header['content'] = content
                header['page_number'] = page_number
                header['base_confidence'] = float(h_base_confidence)
                header['postprocessing_confidence'] = float(h_postprocessing_confidence)
                header['cls'] = cls
                pdf_name = pdf_name
                break
            children = []
            where_clause = ''
            where=[]
            base_query = "select page_objects.id, page_objects.bytes, page_objects.content, page_objects.cls, pages.page_number, pdfs.pdf_name, page_objects.bounding_box, page_objects.confidence, JSON_EXTRACT(page_objects.init_cls_confidences, '$[0][0]') from pages, page_objects, object_contexts as oc, pdfs where oc.id = :oid and page_objects.context_id=oc.id and page_objects.page_id=pages.id and pages.pdf_id=pdfs.id "
            if obj_type is not None:
                where.append("(page_objects.cls=:obj_type or page_objects.cls='Figure Caption' or page_objects.cls='Table Caption')")
            if postprocessing_confidence is not None:
                where.append('page_objects.confidence>=:postprocessing_confidence')
            if base_confidence is not None:
                where.append("JSON_EXTRACT(page_objects.init_cls_confidences, '$[0][0]')>=:base_confidence")
            if len(where) > 0:
                where_clause = ' and ' + ' and '.join(where)
#            logging.info(where_clause)

            q = text(base_query + where_clause)
            res2 = conn.execute(q, oid=obj, obj_type=obj_type, postprocessing_confidence=postprocessing_confidence, base_confidence=base_confidence)
            for id, bytes, content, cls, page_number, pname, bounding_box, o_postprocessing_confidence, o_base_confidence in res2:
                # Filter area because doing the area calculation to the bounding_boxes within mysql is causing me headaches
                if area is not None and 'Caption' not in cls: # don't filter out small captions
                    bounding_box = json.loads(bounding_box)
                    tlx, tly, brx, bry = bounding_box
                    obj_area = (brx - tlx) * (bry - tly)
                    if obj_area < area:
                        logging.info("Object too small")
                        continue
                o = {'id': id, 'bytes': base64.b64encode(bytes).decode('utf-8'), 'content': content, 'page_number': page_number, 'cls': cls, 'postprocessing_confidence' : float(o_postprocessing_confidence), 'base_confidence' : float(o_base_confidence)}
                if ignore_bytes: del(o['bytes'])
                if pdf_name is None:
                    pdf_name = pname
                children.append(o)
            if header['id'] is None and children == []:
                continue
            if children != []: # Don't add headers with no children of interest.
                t = {'header': header,
                        'pdf_name': pdf_name,
                        'children': children,
                        'context_id': context_id}
                if ignore_bytes: del(t['header']['bytes'])
                objects.append(t)
                if len(objects) == 10 and not subselect: # shortcircuit -- we have our 10 to return
                    logging.info(f"10th object for this batch found on context index {context_index[ind]}")
                    query_map[query_name][page_num] = context_index[ind]
                    logging.info(query_map)
                    break
        logging.info(f"{len(objects)} total results pass filter checks")
        if request.endpoint == 'count':
            final_obj = {'total_results' : len(objects)}
        else:
            if subselect:
                final_obj = {'page': page_num,
                        'objects': objects[page_num*10:(page_num+1)*10],
                        }
            else:
                final_obj = {'page': page_num,
                        'objects': objects,
                        }
            for obj in final_obj['objects']:
                obj['bibjson'] = get_bibjson(obj['pdf_name'])
        return jsonify(final_obj)
    except TypeError as e:
        logging.info(f'{e}')
        abort(400)
    finally:
        conn.close()

@app.route('/api/v1/statistics')
def get_stats():
    try:
        conn = engine.connect()
        header_q = text("SELECT count(distinct(page_objects.id)), count(distinct(pages.id)), count(distinct(pdfs.id)) FROM page_objects, pages, pdfs WHERE pages.id=page_objects.page_id AND pdfs.id=pages.pdf_id AND dataset_id=:dataset_id;")
        res1 = conn.execute(header_q, dataset_id=dataset_id)
        for n_po, n_pages, n_pdfs in res1:
            t = {"n_objects" : n_po,
                    "n_pages" : n_pages,
                    "n_pdfs" : n_pdfs
                    }
        return jsonify(t)
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

@app.route('/api/v1/search_es_objects')
def search_es():
    session = Session()
    try:
        _id = request.args.get('id', '')
        obj_type = request.args.get('type', '')
        query = request.args.get('query', '')
        area = request.args.get('area', '')
        ignore_bytes = request.args.get('ignore_bytes', type=bool)
        base_confidence = request.args.get('base_confidence', '')
        postprocess_confidence = request.args.get('postprocessing_confidence', '')
        page_num = int(request.args.get('page', 0))
        offset = int(request.args.get('offset', 0))
        logging.info(f"offset is {offset} and page_num is {page_num}")
        if offset != 0 and page_num == 0:
            page_num = int(offset/10)
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
#                if not obj_type.endswith("Context"):
#                    obj_type += "Context"
                logging.info(f"querying for cls: {obj_type}")
                q = q & Q('match_phrase', cls=obj_type)
                if obj_type in ["Figure", "Table"]:
                    q = q & ~Q('match_phrase', cls="Caption")
            if area != '':
                logging.info(f"querying for area: gte {area}")
                q = q & Q('range', area={'gte': area})
            if base_confidence != '':
                logging.info(f"querying for base_confidence: gte {base_confidence}")
                q = q & Q('range', base_confidence={'gte': base_confidence})
            if postprocess_confidence != '':
                logging.info(f"querying for postprocessing_confidence: gte {postprocess_confidence}")
                q = q & Q('range', postprocessing_confidence={'gte': postprocess_confidence})

            logging.info(f"Filtering to dataset_id {dataset_id}")
            q = q & Q("match_phrase", dataset_id=dataset_id)

            s = Search(index='object').query(q)
            n_results = s.count()
            cur_page = page_num

            logging.info(f"{n_results} total results")

            logging.info(f"Getting results {page_num*10} to {(page_num+1)*10}")
            s = s[page_num*10:(page_num+1)*10]

            response = s.execute()
            for result in response:
                id = result.meta.id
                res = {}
                logging.info(result['cls'])
                # TODO: don't have the necessarily handle to group by header, etc right now. Just placeholder for the time being.
                tobj = {
                        "header" : {
                            'id': None,
                            'bytes' : None,
                            'content' : None,
                            'page_number' : None,
                            'cls': None,
                            'postprocessing_confidence' : None,
                            'base_confidence' : None
                            },
                        'children' : [],
                        }
                obj_id = result.meta['id']
                po, page_number, pdf_name = session.query(PageObject, Page.page_number, Pdf.pdf_name).filter(PageObject.id == obj_id).filter(PageObject.page_id == Page.id).filter(Page.pdf_id == Pdf.id).first()
                logging.info(po)
                res['id'] = obj_id
                if not ignore_bytes:
                    res['bytes'] = po.bytes
                res['content'] = po.content
                res['cls'] = po.cls
#                res['bounding_box'] = po.bounding_box
                res['page_number'] = page_number
                res['postprocessing_confidence'] = float(po.confidence)
                res['base_confidence'] = float(po.init_cls_confidences[0][0])
                res = postprocess_result(res)
                tobj['children'].append(res)
                tobj['pdf_name'] = pdf_name
                tobj['bibjson'] = get_bibjson(pdf_name)
                result_list.append(tobj)
        else:  # passed in a specific object id
            logging.info("id specified, skipping ES junk")
            po, page_number, pdf_name = session.query(PageObject, Page.page_number, Pdf.pdf_name).filter(PageObject.id == _id).filter(PageObject.page_id == Page.id).filter(Page.pdf_id == Pdf.id).first()
            tobj = {
                    "header" : {
                        'id': None,
                        'bytes' : None,
                        'content' : None,
                        'page_number' : None,
                        'cls': None,
                        'postprocessing_confidence' : None,
                        'base_confidence' : None
                        },
                    'children' : [],
                    }
            res = {
                    'id' : po.id,
#                    'bounding_box' : po.bounding_box,
                    'bytes' : po.bytes,
                    'content' : po.content,
                    'cls' : po.cls,
                    'page_number' : page_number,
                    'postprocessing_confidence' : float(po.confidence),
                    'base_confidence' : float(po.init_cls_confidences[0][0])
                    }
            if res['cls'] == 'Table':
                tb = session.query(Table).filter(Table.page_object_id == _id).first()
                if tb is not None:
                    res['table_df'] = tb.df
            res = postprocess_result(res)
            tobj['children'].append(res)
            tobj['pdf_name'] = pdf_name
            tobj['bibjson'] = get_bibjson(pdf_name)

            result_list.append(tobj)
            n_results = len(tobj['children'])
            cur_page = 0

        result_list = [postprocess_result(r) for r in result_list]


        results_obj = {'total_results' : n_results, 'page': cur_page, 'objects': result_list}
        session.close()
        return jsonify(results_obj)
    except TypeError as e:
        session.close()
        logging.info(f'{e}')
        abort(400)

# Testing
# @app.route('/index')
# def index():
#     return "Hello, World!"
