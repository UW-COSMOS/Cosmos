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
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.debug)

def detect(obj):
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
        keep_bytes = dp.keep_bytes
        engine = create_engine('sqlite:///:memory:', echo=False)  
        Session = sessionmaker()
        Session.configure(bind=engine)
        Base.metadata.create_all(engine)
        session = Session()
        obj['img'] = Image.open(io.BytesIO(base64.b64decode(obj['pad_img'].encode('ASCII')))).convert('RGB')
        if not keep_bytes:
            del obj['pad_img']
        detected_objs, softmax_detected_objs = run_inference(model, [obj], model_config, device_str, session)
        detected_objs = detected_objs['0']
        softmax_detected_objs = softmax_detected_objs['0']
        session.close()
        del obj['img']
        obj['detected_objs'] = detected_objs
        obj['softmax_objs'] = softmax_detected_objs

        return obj
    except Exception as e:
        logging.error(str(e), exc_info=True)
        raise e


