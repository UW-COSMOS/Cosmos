from flask import (
    Blueprint, request, jsonify
)
from extraction.qa_extractor import QAExtractor
import os

bp = Blueprint('extraction', __name__, url_prefix='/api/v1/extraction')
qa = QAExtractor(os.environ['SCHEDULER_ADDRESS'])

@bp.route('/qa', methods=['POST'])
def qa():
    request_json = request.get_json()
    q = request_json.get('question')
    c = request_json.get('document')
    result, score = qa.extract(q, c)
    obj = {'page_answers': [{'answer_str': result, 'likelihood': score}]}
    return jsonify(obj)
