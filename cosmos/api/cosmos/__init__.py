import os
from flask import Flask, jsonify, current_app, request
from flask_cors import CORS
from retrieval.elastic_reranking_retriever import ElasticRerankingRetriever
from retrieval.elastic_retriever import ElasticRetriever
from retrieval.elastic_page_retriever import ElasticPageRetriever
import fasttext
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
    app.retriever = ElasticRetriever(os.environ['ELASTIC_ADDRESS'])
    app.page_retriever = ElasticPageRetriever(os.environ['ELASTIC_ADDRESS'])
    try:
        app.word_embeddings_model = fasttext.load_model('/data/vecs.bin')
    except Exception as e:
        logger.error(f'{e}')
        pass

    from . import retrieval
    app.register_blueprint(retrieval.bp)

    # hack to get url prefixes registered as required/desired IAR - 30.Oct.2020
    if 'PREFIX' in os.environ:
        logging.info(f"Stripping {os.environ['PREFIX']}")
        prefix=os.environ['PREFIX']
    else:
        logging.info("No prefix stripped.")
        prefix=''
    if "API_VERSION" in os.environ:
        api_version=os.environ['API_VERSION']
    else:
        api_version='v2_beta'
    app.register_blueprint(retrieval.bp, url_prefix=f"{prefix}/{api_version}")
    app.register_blueprint(retrieval.bp, url_prefix='/sets/xdd-covid-19/api') # for backward compatibility
    app.register_blueprint(retrieval.bp, url_prefix=f'/sets/xdd-covid-19/api/{api_version}') # for backward compatibility

    #from . import extraction
    #app.register_blueprint(extraction.bp)

    from . import embeddings
    app.register_blueprint(embeddings.bp)
    logger.error(app.url_map)
    CORS(app)

    return app




