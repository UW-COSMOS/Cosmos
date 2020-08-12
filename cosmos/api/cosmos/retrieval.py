from flask import (
    Blueprint, request, jsonify
)
from retrieval.elastic_reranking_retriever import ElasticRerankingRetriever
import os

bp = Blueprint('retrieval', __name__, url_prefix='/api/v1/retrieval')
retriever = ElasticRerankingRetriever(os.environ['SCHEDULER_ADDRESS'])

@bp.route('/query', methods=['GET'])
def query():
    request_json = request.get_json()
    q = request_json.get('q')
    response = {'query': q, 'documents': []}
    results = retriever.search(q)
    docs = [{'sid': d['id'], 'title': d['docname'], 'url': 'example.com', 'blurbs': [{'text': d['context']}]} for d in results]
    response['documents'] = docs
    return jsonify(response)

