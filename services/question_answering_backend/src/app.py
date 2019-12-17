from flask import Flask, request, abort
from infer import infer_qa
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)

app = Flask(__name__)
@app.route('/query')
def query():
    logging.debug("Inside query function")
    try:
        query = request.args.get('query', '')
        candidate = request.args.get('candidate','')
        answer, probability = infer_qa(query, candidate)
        return {"answer":answer, "probability":probability}
    except Exception as e:
        logging.debug(e)
        abort(400)
