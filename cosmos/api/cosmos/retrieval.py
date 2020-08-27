from flask import (
    Blueprint, request, jsonify
)
from retrieval.elastic_reranking_retriever import ElasticRerankingRetriever
import os

bp = Blueprint('retrieval', __name__, url_prefix='/api/v2')
retriever = ElasticRerankingRetriever(os.environ['SCHEDULER_ADDRESS'])

@bp.route('/search', methods=['GET'])
def search():
    query = request.args.get('query', type=str)
    obj_type = request.args.get('type', type=str)
    area = request.args.get('area', type=int)
    page_num = request.args.get('page', type=int)
    ignore_bytes = request.args.get('ignore_bytes', type=bool)
    if page_num is None:
        page_num = 0
    base_confidence = request.args.get('base_confidence', type=float)
    postprocessing_confidence = request.args.get('postprocessing_confidence', type=float)



    request_json = request.get_json()
    q = request_json.get('q')
    response = {'query': q, 'documents': []}
    results = retriever.search(q)
    docs = [{'sid': d['id'], 'title': d['docname'], 'url': 'example.com', 'blurbs': [{'text': d['context']}]} for d in results]
    response['documents'] = docs
    return jsonify(response)

