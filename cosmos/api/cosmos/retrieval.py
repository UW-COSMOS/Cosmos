from flask import (
    Blueprint, request, jsonify, current_app
)
import logging
import os
import requests
import base64
import json
logger = logging.getLogger(__name__)


if 'PREFIX' in os.environ:
    logging.info(f"Stripping {os.environ['PREFIX']}")
    prefix=os.environ['PREFIX']
else:
    logging.info("No prefix stripped.")
    prefix=''
bp = Blueprint('retrieval', __name__, url_prefix=f'{prefix}/')
if "API_VERSION" in os.environ:
    VERSION=os.environ['API_VERSION']
else:
    VERSION='v2_beta'

parameter_defs = {
        'query': '(str, Required) - term or comma-separated list of terms to search for. Default search logic will utilize an OR of comma- or space-separated words.',
        'type': '[Table, Figure, Equation, Body Text, Combined] - the type of object to search for.',
        'page': '(int) - Page of results (starts at 0)',
        'inclusive': '(bool) - Changes default query search to apply AND logic to comma- or space-separated words.',
        'base_confidence': '(float)- Output logit score from detection model. Measures confidence of the initial COSMOS classification. Only results with confidence higher than the specified value will be returned. Default is 1.0.',
        'postprocessing_confidence': '(0.0-1.0) - Confidence score of the COSMOS post-processing model. Only results with confidence higher than the specified value will be returned. Default is 0.7.',
        'document_filter_terms': '(str) - Comma- or space-separated list of additional terms to require at the document level. Applies AND logic to comma- or space-separated words.',
        'context_filter_terms': '(str) - Comma- or space-separated list of additional terms to require at the object level. Applies AND logic to comma- or space-separated words.',
        'ignore_bytes': '(bool) If true, do not return the bytes of the extracted image (e.g. only return text content of objects)'
        }

fields_defs = {
        "page" : "Current results page number",
        "total" : "The total number of objects matching the query",
        "v" : "API version",
        "pdf_name" : "Filename of documents",
        "context_id" : "Internal COSMOS id of aggregated context.",
        "context_content" : "Complete content of aggregated context.",
        "context_summary" : "Summarized text of aggregated context.",
        "context_keywords" : "Computed keywords of aggregated context.",
        "bibjson" : "Bibliographical JSON of document (looked up within xDD)",
        "[header/child/object].id" : "Internal COSMOS id of object",
        "[header/child/object].bytes" : "base64 ASCII-decoded image bytes of the object",
        "[header/child/object].content" : "Text content within the object",
        "[header/child/object].page_number" : "Source page number within the document",
        "[header/child/object].cls" : "COSMOS-computed class of the object",
        "[header/child/object].base_confidence" : "Confidence score (logit) of the initial COSMOS classification.",
        "[header/child/object].postprocessing_confidence" : "Confidence score of the COSMOS post-processing model."
        }



def get_bibjsons(pdf_names):
    docids=','.join(pdf_names)
    resp = requests.get(f"https://xdd.wisc.edu/api/articles?docids={docids}")
    bibjson = {}
    if resp.status_code == 200:
        data = resp.json()
        if 'success' in data:
            for i in data['success']['data']:
                bibjson[i['_gddid']] = i
        else:
            current_app.logger.error(f'Unable to find success key: {data}')
            bibjson = None #{"Error" : "Could not retrieve article data"}
    else:
        bibjson = None #{"Error" : "Could not retrieve article data"}
    return bibjson

def get_bibjson(pdf_name):
    xdd_docid = pdf_name.replace(".pdf", "")
    if 'full' in xdd_docid:
        xdd_docid = xdd_docid.replace("v1.full", "")
        resp = requests.get(f"https://xdd.wisc.edu/api/articles?doi={xdd_docid}")
    else:
        resp = requests.get(f"https://xdd.wisc.edu/api/articles?docid={xdd_docid}")
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

@bp.route('/', defaults={'path': ''})
@bp.route('/<path:path>')
def help(path):
    helptext = {
            "success" : {
                "v" : VERSION,
                "description" : "COSMOS extraction search API",
                "routes" : {
                    f"/api/{VERSION}/search" : "Query the COSMOS extractions for objects and contexts mentioning a term passing filtration criteria. Utilizes the Anserini retrieval engine. Objects matching the query are returned, along with their parent or children objects resulting from the COSMOS contextual aggregation process (e.g. figures will be return as a child object for a figure caption mentioning a phrase; all body text within a section will be returned as children to a section header mentioning a term).",
                    f"/api/{VERSION}/count" : "Provides the number of COSMOS extractions matching the specified query.",
        }
        }
        }

    return jsonify(helptext)

def makedict(keys, map_dict):
    t = {}
    for i in keys:
        t[i] = map_dict[i]
    return t


def route_help(endpoint):
    helptext = {}
    endpoint = endpoint.split(".")[-1]
    if endpoint == "count" or endpoint == "search":
        helptext = {
                "success": {
                    "v" : VERSION,
                    "description" : "Query the COSMOS extractions for objects and contexts mentioning a term passing filtration criteria. Utilizes the Elasticsearch retrieval engine. Objects matching the query are returned, along with their parent or children objects resulting from the COSMOS contextual aggregation process (e.g. figures will be return as a child object for a figure caption mentioning a phrase; all body text within a section will be returned as children to a section header mentioning a term). Result order is determined by search rank (results with high-density mentions of the term will appear first). 30 results are returned per page.",
                    'options': {
                        'parameters' : makedict(['query', 'type', 'page', 'inclusive', 'base_confidence', 'postprocessing_confidence', 'ignore_bytes'], parameter_defs),
                        'output_formats' : 'json',
                        'examples' : [
                            f'/api/{VERSION}/search?query=temperature&type=Figure&base_confidence=1.0&postprocessing_confidence=0.7',
                            f'/api/{VERSION}/search?query=remdesevir,chloroquine&inclusive=true&base_confidence=1.0&postprocessing_confidence=0.7',
                            f'/api/{VERSION}/search?query=ACE2&type=Table&base_confidence=1.0&postprocessing_confidence=0.7&document_filter_terms=covid-19'
                            ],
                        'fields' : makedict(["page", "total", "v", "pdf_name","context_id","context_content","context_summary","context_keywords","bibjson","[header/child/object].id","[header/child/object].bytes","[header/child/object].content","[header/child/object].page_number","[header/child/object].cls","[header/child/object].base_confidence","[header/child/object].postprocessing_confidence"], fields_defs)
                        }
                    }
                }
        if endpoint=="count":
            helptext['success']['fields'] = {"total_results" : "Total number of objects matching the search criteria"}
    return helptext

@bp.route(f'/search/image/<page_id>')
@bp.route(f'/page/<page_id>')
def page(page_id):
    current_app.logger.info("Calling page_retriever.search()")
    resp = current_app.page_retriever.search(page_id)
    current_app.logger.info("Called page_retriever.search()")
    return jsonify({'results' : resp})

@bp.route('/search/tags/all')
def tags():
    '''
    hardcode the tags for the time being
    '''
    resp = {"v":1,"license":"MIT","data":[{"tag_id":1,"name":"Body Text","description":"The primary text of an article","color":"#aaaaaa","created":"2019-04-02T20:04:30.849Z"},{"tag_id":2,"name":"Figure","description":"A chart, graph, or other graphical display","color":"#a15231","created":"2019-04-02T20:04:30.849Z"},{"tag_id":3,"name":"Figure Note","description":"A footnote explanation of specific content in a figure","color":"#801515","created":"2019-04-02T20:04:30.849Z"},{"tag_id":4,"name":"Figure Caption","description":"A text description associated with an entire figure","color":"#c45778","created":"2019-04-02T20:04:30.849Z"},{"tag_id":5,"name":"Table","description":"A tabular representation of information","color":"#432F75","created":"2019-04-02T20:04:30.849Z"},{"tag_id":6,"name":"Table Note","description":"A footnote to explain a subset of table content","color":"#162c57","created":"2019-04-02T20:04:30.849Z"},{"tag_id":7,"name":"Table Caption","description":"A text description associated with an entire table","color":"#73548f","created":"2019-04-02T20:04:30.849Z"},{"tag_id":8,"name":"Page Header","description":"Document-wide summary information, including page no., at top of page","color":"#2a7534","created":"2019-04-02T20:04:30.849Z"},{"tag_id":9,"name":"Page Footer","description":"Document-wide summary information, including page no., at bottom of page","color":"#345455","created":"2019-04-02T20:04:30.849Z"},{"tag_id":10,"name":"Section Header","description":"Text identifying section within text of document","color":"#1aa778","created":"2019-04-02T20:04:30.849Z"},{"tag_id":11,"name":"Equation","description":"An equation","color":"#2C4770","created":"2019-04-02T20:04:30.849Z"},{"tag_id":12,"name":"Equation label","description":"An identifier for an equation","color":"#4D658D","created":"2019-04-02T20:04:30.849Z"},{"tag_id":13,"name":"Abstract","description":"Abstract of paper","color":"#D4A26A","created":"2019-04-02T20:04:30.849Z"},{"tag_id":14,"name":"Reference text","description":"References to other works","color":"#804D15","created":"2019-04-02T20:04:30.849Z"},{"tag_id":15,"name":"Other","description":"Textual metadata and image content that is not semantically meaningful","color":"#96990c","created":"2019-04-02T20:04:30.849Z"},{"tag_id":16,"name":"Equation definition","description":"An equation definition","color":"#23477e","created":"2019-04-02T20:04:30.849Z"},{"tag_id":17,"name":"Symbol","description":"A symbol","color":"#4c2c70","created":"2019-04-02T20:04:30.849Z"},{"tag_id":18,"name":"Symbol definition","description":"A symbol definition","color":"#ff0000","created":"2019-04-02T20:04:30.849Z"}]}
    return jsonify(resp)

@bp.route(f'/count', endpoint='count')
@bp.route(f'/search', endpoint='search')
def search():
    if len(request.args) == 0:
        return jsonify(route_help(request.endpoint))
    current_app.logger.info("args:")
    current_app.logger.info(request.args)
    query = request.args.get('query', type=str)
    obj_type = request.args.get('type', type=str)
    inclusive = request.args.get('inclusive', default=False, type=bool)

    document_filter_terms = request.args.get('document_filter_terms', default='', type=str).split(',')
    if document_filter_terms == ['']: document_filter_terms=[]
    # TODO: implement this.
    context_filter_terms = request.args.get('context_filter_terms', default='', type=str).split(',')
    if context_filter_terms == ['']: context_filter_terms=[]

    docids = request.args.get('docids', default='', type=str).split(',')
    if docids == ['']: docids=[]

    if obj_type == 'Body Text':
        obj_type = 'Section'
    page_num = request.args.get('page', type=int)
    ignore_bytes = request.args.get('ignore_bytes', type=bool)
    if page_num is None:
        page_num = 0
    base_confidence = request.args.get('base_confidence', default=1.0, type=float)
    postprocessing_confidence = request.args.get('postprocessing_confidence', default=0.7, type=float)
    current_app.logger.error('Received search query. Starting search.')

    count = current_app.retriever.search(query, ndocs=30, page=page_num, cls=obj_type,
                                               detect_min=base_confidence, postprocess_min=postprocessing_confidence,
                                               get_count=True, final=False, inclusive=inclusive, document_filter_terms=document_filter_terms, docids=docids)
    if 'count' in request.endpoint:
        return jsonify({'total_results': count, 'v': VERSION})
    current_app.logger.info(f"page: {page_num}, cls: {obj_type}, detect_min: {base_confidence}, postprocess_min: {postprocessing_confidence}")
    current_app.logger.info(f"Passing in {document_filter_terms}")
    results = current_app.retriever.search(query, ndocs=30, page=page_num, cls=obj_type,
                                         detect_min=base_confidence, postprocess_min=postprocessing_confidence, get_count=False, final=True, inclusive=inclusive, document_filter_terms=document_filter_terms, docids=docids)
    if len(results) == 0:
        return {'page': 0, 'objects': [], 'v': VERSION}
    image_dir = '/data/images'
    bibjsons = get_bibjsons([i['pdf_name'].replace(".pdf", "")  for i in results])
    for result in results:
        result['bibjson'] = bibjsons[result['pdf_name'].replace(".pdf", "")]
        for child in result['children']:
            if child['bytes'] is not None and not ignore_bytes:
                img_pth = os.path.basename(child['bytes'])
                img_pth = img_pth[:2] + "/" + os.path.basename(child['bytes']) # hack. Reorganized images into filename[:2]/filename because having half a million pngs in one dir suuuuuuucks
                with open(os.path.join(image_dir, img_pth), 'rb') as imf:
                    child['bytes'] = base64.b64encode(imf.read()).decode('ascii')
            else:
                child['bytes'] = None

    return jsonify({'v' : VERSION, 'total': count, 'page': page_num, 'objects': results})

@bp.route('object/<objid>')
def object(objid):
    ignore_bytes = request.args.get('ignore_bytes', type=bool)
    contexts = [current_app.retriever.get_object(objid)]
    count = len(contexts)
    page_num = 0
    results = [
        {
            'header': {},
            'pdf_name': obj.pdf_name,
            'children': [{
                'id': obj.meta.id,
                'bytes': obj.img_pth,
                'cls': obj.cls,
                'postprocessing_confidence': obj.postprocess_score,
                'base_confidence': obj.detect_score,
                'content': obj.content,
                'header_content': obj.header_content,
            }],
            'context_keywords': '',
            'context_summary': '',
            'context_content': '',
            'context_id': obj.meta.id
        } for obj in contexts
    ]
    if len(results) == 0:
        return {'page': 0, 'objects': [], 'v': VERSION}
    image_dir = '/data/images'
    bibjsons = get_bibjsons([i['pdf_name'].replace(".pdf", "")  for i in results])
    for result in results:
        result['bibjson'] = bibjsons[result['pdf_name'].replace(".pdf", "")]
        for child in result['children']:
            if child['bytes'] is not None and not ignore_bytes:
                img_pth = os.path.basename(child['bytes'])
                img_pth = img_pth[:2] + "/" + os.path.basename(child['bytes']) # hack. Reorganized images into filename[:2]/filename because having half a million pngs in one dir suuuuuuucks
                with open(os.path.join(image_dir, img_pth), 'rb') as imf:
                    child['bytes'] = base64.b64encode(imf.read()).decode('ascii')
            else:
                child['bytes'] = None
    return jsonify({'v' : VERSION, 'total': count, 'page': page_num, 'objects': results})


@bp.route(f'/statistics', endpoint='statistics', methods=['GET'])
def statistics():
    return jsonify({'n_pages': current_app.retriever.count("page"), 'n_objects': current_app.retriever.count("object"), 'n_pdfs': current_app.retriever.count("fulldocument")})
