import io
from PIL import Image
from ingest.process.detection.src.infer import run_inference
from ingest.process.detection.src.torch_model.train.data_layer.sql_types import Base
import logging
import torch
import json
import base64
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import os
from dask.distributed import get_worker
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
import pickle

def detect(pkl_path):
    with open(pkl_path, 'rb') as rf:
        obj = pickle.load(rf)
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'DetectPlugin' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise Exception('No detect plugin registered')

        model = dp.model
        model_config = dp.model_config
        device_str = dp.device_str
        engine = create_engine('sqlite:///:memory:', echo=False)  
        Session = sessionmaker()
        Session.configure(bind=engine)
        Base.metadata.create_all(engine)
        session = Session()
        detect_obj = {'id': '0', 'proposals': obj['proposals']}
        if type(obj['pad_img']) == str:
            detect_obj['img'] = Image.open(obj['pad_img']).convert('RGB')
        else:
            detect_obj['img'] = Image.open(io.BytesIO(base64.b64decode(obj['pad_img'].encode('ASCII')))).convert('RGB')
        detected_objs, softmax_detected_objs = run_inference(model, [detect_obj], model_config, device_str, session)
        detected_objs = detected_objs['0']
        softmax_detected_objs = softmax_detected_objs['0']
        session.close()
        obj['detected_objs'] = detected_objs
        obj['softmax_objs'] = softmax_detected_objs
        with open(pkl_path, 'wb') as wf:
            pickle.dump(obj, wf)
        return pkl_path
    except Exception as e:
        logging.error(str(e), exc_info=True)
        raise e


