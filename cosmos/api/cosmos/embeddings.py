from flask import (
    Blueprint, request, current_app, abort, jsonify
)

bp = Blueprint('embeddings', __name__, url_prefix='')

@bp.route('/api/v1/word2vec', endpoint='word2vec', methods=['GET'])
def word2vec():
    query_word= request.values.get('word')
    n_responses= int(request.values.get('n', '10'))

    if not hasattr(current_app, 'word_embeddings_model'):
        abort(500)

    if not query_word:
        abort(400)

    results = current_app.word_embeddings_model.get_nearest_neighbors(query_word, k=n_responses)
    # Reverse words and scores
    scores, words = zip(*results)
    results = list(zip(words, scores))
    return jsonify(results)



