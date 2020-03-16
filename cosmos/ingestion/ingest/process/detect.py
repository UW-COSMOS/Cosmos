import io
from PIL import Image
import falcon
from ingest.process.detection.src.infer import run_inference, get_model
from ingest.process.detection.src.torch_model.train.data_layer.sql_types import Base
import logging
import json
import base64
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)

engine = create_engine('sqlite:///:memory:', echo=False)
Session = sessionmaker()
Session.configure(bind=engine)

device_str = 'cuda'
cfg_path = '/process/configs/model_config.yaml'
weights_pth = '/process/weights/model_weights.pth'
model = get_model(cfg_path, weights_pth, device_str)

class Detect(object):

    def __init__(self, config_pth, device_str):
        self.model_config = config_pth
        self.device_str = device_str

    def on_post(self, req, resp):
        session = Session()
        Base.metadata.create_all(engine)
        obj = req.media
        obj['img'] = Image.open(io.BytesIO(base64.b64decode(obj['img'].encode('ASCII')))).convert('RGB')
        logging.info("Running inference.")
        detected_objs = run_inference(model, [obj], self.model_config, self.device_str, session)['0']
        logging.info(f"Inference complete. {len(detected_objs)} objects detected.")
        session.close()
        return detected_objs


api = application = falcon.API()
d = Detect(cfg_path, device_str)
api.add_route('/detect', d)
