from flask import (
    Blueprint, request, jsonify, current_app
)
import logging
import os
import requests
import base64
import json
logger = logging.getLogger(__name__)

bp = Blueprint('retrieval', __name__, url_prefix='/api/v1/')


def get_bibjson(pdf_name):
    xdd_docid = pdf_name.replace(".pdf", "")
    logger.info(f"Getting bibjson for {xdd_docid}")
    if 'full' in xdd_docid:
        xdd_docid = xdd_docid.replace("v1.full", "")
        resp = requests.get(f"https://geodeepdive.org/api/articles?doi={xdd_docid}")
    else:
        resp = requests.get(f"https://geodeepdive.org/api/articles?docid={xdd_docid}")
    if resp.status_code == 200:
        data = resp.json()
        if 'success' in data:
            bibjson = data["success"]["data"][0]
        else:
            current_app.logger.error(f'Unable to find success key: {data}')
            bibjson = None #{"Error" : "Could not retrieve article data"}
    else:
        bibjson = None #{"Error" : "Could not retrieve article data"}
    return bibjson


@bp.route('/count', endpoint='count', methods=['GET'])
@bp.route('/search', endpoint='search', methods=['GET'])
def search():
    query = request.args.get('query', type=str)
    obj_type = request.args.get('type', type=str)
    if obj_type == 'Body Text':
        obj_type = 'Section'
    area = request.args.get('area', type=int)
    page_num = request.args.get('page', type=int)
    ignore_bytes = request.args.get('ignore_bytes', type=bool)
    if page_num is None:
        page_num = 0
    base_confidence = request.args.get('base_confidence', type=float)
    postprocessing_confidence = request.args.get('postprocessing_confidence', type=float)
    current_app.logger.error('Received search query. Starting search.')


    if request.endpoint == 'count':
        #return jsonify({'total_results': 1})
        count = current_app.retriever.search(query, ndocs=30, page=page_num, cls=obj_type,
                                               detect_min=base_confidence, postprocess_min=postprocessing_confidence,
                                               get_count=True)
        return jsonify({'total_results': count})
    with open('/test/search.json', 'r') as rf:
        o = json.load(rf)
    results = current_app.retriever.search(query, ndocs=30, page=page_num, cls=obj_type,
                                         detect_min=base_confidence, postprocess_min=postprocessing_confidence)
    if len(results) == 0:
        return {'page': 0, 'objects': []}
    image_dir = '/images'
    for result in results:
        bjson = get_bibjson(result['pdf_name'])
        if bjson is None:
            bjson = o['objects'][0]['bibjson']
        result['bibjson'] = bjson #get_bibjson(result['pdf_name'])

        for child in result['children']:
            if child['bytes'] is not None:
                img_pth = os.path.basename(child['bytes'])
                with open(os.path.join(image_dir, img_pth), 'rb') as imf:
                    child['bytes'] = base64.b64encode(imf.read()).decode('ascii')
            else:
                child['bytes'] = o['objects'][0]['children'][0]['bytes']

    return jsonify({'page': page_num, 'objects': results})

@bp.route('/statistics', endpoint='statistics', methods=['GET'])
def statistics():
    return jsonify({'n_objects': 1, 'n_pages': 1, 'n_pdfs': 1})
