from ingest.process.postprocess.xgboost_model.featurizer import load_data_objs
import yaml
import joblib
import xgboost
import numpy as np
import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.ERROR)


def run_inference(model, classes, page_objs):
    # TODO: WRITE TEST CASE
    if len(page_objs) == 0:
        logger.error('run_inference (postprocess) was passed 0 page_objs')
        return ''

    p_bb, _, texts = zip(*page_objs)

    objs = load_data_objs(page_objs, classes)
    prob = model.predict_proba(objs)

    pred_scores = np.max(prob, axis=1).tolist()
    pred_idxs = np.argmax(prob, axis=1)
    pred_cls = [classes[p] for p in pred_idxs]
    return list(zip(p_bb, pred_cls, texts, pred_scores))

