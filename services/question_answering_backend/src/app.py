from flask import Flask, request, abort
from infer import infer_qa

app = Flask(__name__)
@app.route('/query')
def query():
    try:
        query = request.args.get('query', '')
	candidate = request.args.get('candidate','')
	return infer_qa(query, candidate)
    except:
        abort(400)
