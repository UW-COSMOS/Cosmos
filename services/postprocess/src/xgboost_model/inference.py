from xgboost_model.featurizer import load_data_objs
import yaml
import joblib
import xgboost

with open("classes.yaml") as stream:
    classes  = yaml.load(stream)["classes"]

def run_inference(page_objs, weights_pth):
    model = joblib.load(weights_pth)
    objs = load_data_objs(page_objs, classes)
    pred = model.predict(objs)
    page_objs["pp_detected_obj"] = [classes[p] for p in pred]
    return page_objs
