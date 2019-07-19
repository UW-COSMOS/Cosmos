from xgboost_model.featurizer import load_data_objs
import yaml
import joblib
import xgboost

with open("classes.yaml") as stream:
    classes  = yaml.load(stream)["classes"]

def run_inference(page_objs, weights_pth):
    predict_list = page_objs['ocr_detected_objs']
    p_bb, _, _ = zip(*predict_list)
    model = joblib.load(weights_pth)
    objs = load_data_objs(page_objs, classes)
    pred = model.predict(objs)
    predictions = [classes[p] for p in pred]
    page_objs["pp_detected_obj"] = list(zip(p_bb, predictions))
    return page_objs
