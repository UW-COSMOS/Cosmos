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
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)

app = Flask(__name__)
engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
dataset_id = os.environ['DATASET_ID']

retrievers = {}

for ctype in ["Section", "FigureContext", "TableContext", "EquationContext", "Combined"]:
    doc_idx_path = os.path.join('/index_dir/', dataset_id, ctype, 'documents_jsonl/lucene-index')
    logging.info('/index_dir/' + dataset_id +  ctype + 'documents_jsonl/lucene-index')
    logging.info(os.path.join('/index_dir/', dataset_id, ctype, 'documents_jsonl/lucene-index'))
    context_idx_pth = os.path.join('/index_dir/', dataset_id, ctype, 'contexts_jsonl/lucene-index')
    doc2odoc_pth = os.path.join('/index_dir/', dataset_id, ctype, 'id_to_pdf.pkl')
    ctx2octx_pth = os.path.join('/index_dir/', dataset_id, ctype, 'id_to_context.pkl')
    octx2odoc_pth = os.path.join('/index_dir/', dataset_id, ctype, 'context_to_doc.pkl')
    logging.info(f"Loading retriever for {dataset_id} -- {ctype} ({doc_idx_path}, {context_idx_pth}, {doc2odoc_pth}, {ctx2octx_pth}, {octx2odoc_pth}, k1=1000, k2=1000)")
    retrievers[ctype] = Retrieval(doc_idx_path, context_idx_pth, doc2odoc_pth, ctx2octx_pth, octx2odoc_pth, k1=1000, k2=1000)

objtype_index_map = {
        "Figure" : "FigureContext",
        "Equation" : "EquationContext",
        "Table" : "TableContext",
        "Body Text" : "Section",
        }

@app.route('/api/v1/search')
def search():
    conn = engine.connect()
    try:
        query = request.args.get('query', type=str)
        obj_type = request.args.get('type', type=str)
        area = request.args.get('area', type=int)
        page_num = request.args.get('page', type=int)
        if page_num is None:
            page_num = 0
        base_confidence = request.args.get('base_confidence', type=float)
        postprocessing_confidence = request.args.get('postprocessing_confidence', type=float)
        logging.info(f"Using {objtype_index_map.get(obj_type, 'Combined')} retriever")
        retriever = retrievers[objtype_index_map.get(obj_type, "Combined")]
        results = retriever.search(query)
        obj_ids = [r[1] for r in results]
        logging.info(f"{len(obj_ids)} matching contexts found!")
        if len(obj_ids) == 0:
            return {'page': 0, 'objects': []}
        objects = []
        for ind, obj in enumerate(obj_ids):
            header_q = text('select page_objects.id, page_objects.bytes, page_objects.content, page_objects.cls, pages.page_number, pdfs.pdf_name from pages, page_objects, object_contexts as oc, pdfs where oc.id = :oid and page_objects.id=oc.header_id and page_objects.page_id=pages.id and pdfs.id=oc.pdf_id;')
            res1 = conn.execute(header_q, oid=obj)
            header_id = None
            header_bytes = None
            header_content = None
            header_page_number = None
            header_cls = None
            pdf_name = None
            context_id = obj
            for id, bytes, content, cls, page_number, pdf_name in res1:
                header_id = id
                header_bytes = base64.b64encode(bytes).decode('utf-8')
                header_content = content
                header_page_number = page_number
                header_cls = cls
                pdf_name = pdf_name
                break
            children = []
            where_clause = ''
            where=[]
            base_query = 'select page_objects.id, page_objects.bytes, page_objects.content, page_objects.cls, pages.page_number, pdfs.pdf_name, page_objects.bounding_box from pages, page_objects, object_contexts as oc, pdfs where oc.id = :oid and page_objects.context_id=oc.id and page_objects.page_id=pages.id and pages.pdf_id=pdfs.id '
            if obj_type is not None:
                where.append('page_objects.cls=:obj_type')
            if postprocessing_confidence is not None:
                where.append('page_objects.confidence>=:postprocessing_confidence')
            if base_confidence is not None:
                where.append("JSON_EXTRACT(page_objects.init_cls_confidences, '$[0][0]')>=:base_confidence")
            if len(where) > 0:
                where_clause = ' and ' + ' and '.join(where)
            logging.info(where_clause)
            q = text(base_query + where_clause)
            res2 = conn.execute(q, oid=obj, obj_type=obj_type, postprocessing_confidence=postprocessing_confidence, base_confidence=base_confidence)
            for id, bytes, content, cls, page_number, pname, bounding_box in res2:
                # Filter area because doing the area calculation to the bounding_boxes within mysql is causing me headaches
                if area is not None:
                    bounding_box = json.loads(bounding_box)
                    tlx, tly, brx, bry = bounding_box
                    obj_area = (brx - tlx) * (bry - tly)
                    if obj_area < area:
                        continue
                o = {'id': id, 'bytes': base64.b64encode(bytes).decode('utf-8'), 'content': content, 'page_number': page_number, 'cls': cls}
                if pdf_name is None:
                    pdf_name = pname
                children.append(o)
            if header_id is None and children == []:
                continue
            if children != []: # Don't add headers with no children of interest.
                objects.append({'header_id': header_id,
                                'header_bytes': header_bytes,
                                'header_content': header_content,
                                'header_cls': header_cls,
                                'pdf_name': pdf_name,
                                'children': children,
                                'context_id': context_id})
        logging.info(f"{len(objects)} total results")
        logging.info(f"Getting results {page_num*20} to {(page_num+1)*20}")
        final_obj = {'page': 0,
                'objects': objects[page_num*20:(page_num+1)*20],
                'total__results': len(objects)}
        return jsonify(final_obj)
    except TypeError as e:
        logging.info(f'{e}')
        abort(400)
    finally:
        conn.close()

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
