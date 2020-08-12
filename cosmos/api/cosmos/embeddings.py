from flask import (
    Blueprint, flash, g, redirect, render_template, request, session, url_for
)

bp = Blueprint('embeddings', __name__, url_prefix='/api/v1/embeddings')
