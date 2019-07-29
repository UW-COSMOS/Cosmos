from xgboost_model.featurizer import load_data_objs
import yaml
import joblib
import xgboost
import numpy as np
import logging 
with open("classes.yaml") as stream:
    classes  = yaml.load(stream)["classes"]

def run_inference(page_objs, weights_pth):
    if 'ocr_detected_objs' not in page_objs:
        logging.info('This page has not had ocr or has no ocr_detected objects: {page_objs["_id"]}')
        page_objs["pp_detected_objs"] = []
    elif len(page_objs['ocr_detected_objs']) == 0:
        logging.info('This page has no ocr_detected objects: {page_objs["_id"]}')
        page_objs["pp_detected_objs"] = []
    else:    
        predict_list = page_objs['ocr_detected_objs']
        p_bb, _, _ = zip(*predict_list)

        objs = load_data_objs(page_objs, classes)
        
        model = joblib.load(weights_pth)
        prob = model.predict_proba(objs)

        pred_scores = np.max(prob, axis=1).tolist()
        pred_idxs = np.argmax(prob, axis=1)
        pred_cls = [classes[p] for p in pred_idxs]
        page_objs["pp_detected_objs"] = list(zip(p_bb, pred_cls, pred_scores))

    return page_objs