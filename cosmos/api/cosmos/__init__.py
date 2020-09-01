import os
from flask import Flask, jsonify, current_app, request
from retrieval.elastic_reranking_retriever import ElasticRerankingRetriever
import logging
import requests
logger = logging.getLogger(__name__)

def create_app():
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
    )
    app.debug = True
    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass
    app.retriever = ElasticRerankingRetriever(os.environ['SCHEDULER_ADDRESS'])

    from . import retrieval
    app.register_blueprint(retrieval.bp)

    #from . import extraction
    #app.register_blueprint(extraction.bp)

    #from . import embeddings
    #app.register_blueprint(embeddings.bp)
    logger.error(app.url_map)

    return app




