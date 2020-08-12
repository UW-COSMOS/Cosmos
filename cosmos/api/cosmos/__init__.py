import os
from flask import Flask

def create_app():
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
    )
    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    from . import retrieval
    app.register_blueprint(retrieval.bp)

    from . import extraction
    app.register_blueprint(extraction.bp)

    from . import embeddings
    app.register_blueprint(embeddings.bp)

    return app




