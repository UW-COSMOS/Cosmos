
"""
Route to handle decomposition of pages into page objects
"""
import os
from PIL import Image
from ingest.utils.visualize import write_regions
from ingest.process.proposals.connected_components import get_proposals
from ingest.process.detection.src.preprocess import pad_image
from ingest.process.postprocess.xgboost_model.inference import run_inference as postprocess
from ingest.process.postprocess.pp_rules import apply_rules as postprocess_rules
from dask.distributed import get_worker
import pickle
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.INFO)
logging.getLogger("pdfminer").setLevel(logging.DEBUG)
logging.getLogger("PIL").setLevel(logging.DEBUG)
logger = logging.getLogger()
logger.setLevel(logging.INFO)


def propose_and_pad(obj, visualize=False):
    tmp_dir, pdf_name, page_num = obj
    pkl_path = f'{os.path.join(tmp_dir, pdf_name)}_{page_num}.pkl'
    image_path = f'{os.path.join(tmp_dir, pdf_name)}_{page_num}'
    img = Image.open(image_path).convert('RGB')
    with open(pkl_path, 'rb') as rf:
        try:
            obj = pickle.load(rf)
        except EOFError as e:
            logging.error(e)
            logging.error(f'Pickle path: {pkl_path}')
            raise e
        except pickle.UnpicklingError as e:
            logging.error(e)
            logging.error(f'Pickle path: {pkl_path}')
            raise e
    coords = get_proposals(img)
    padded_img = pad_image(img)
    obj['id'] = '0'
    obj['proposals'] = coords
    if visualize:
        write_regions(image_path, coords)
    obj['page_id'] = f'{pdf_name}_{page_num}'
    obj['page_path'] = f'{tmp_dir}/{obj["page_id"]}'
    d = f'{tmp_dir}/{pdf_name}_{page_num}_pad'
    padded_img.save(d, "PNG")
    obj['pad_img'] = d
    with open(pkl_path, 'wb') as wf:
        pickle.dump(obj, wf)
    return pkl_path


def xgboost_postprocess(pkl_path):
    with open(pkl_path, 'rb') as rf:
        obj = pickle.load(rf)
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            logger.info(plg)
            if 'Process' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise ValueError('No process plugin registered')
    except Exception as e:
        logger.error(str(e), exc_info=True)
        raise e

    objects = obj['content']
    objects = postprocess(dp.postprocess_model, dp.classes, objects)
    # remove empty strings returned from postprocess
    objects = [i for i in objects if i != '']

    obj['xgboost_content'] = objects
    with open(pkl_path, 'wb') as wf:
        pickle.dump(obj, wf)
    return pkl_path


def rules_postprocess(pkl_path):
    with open(pkl_path, 'rb') as rf:
        obj = pickle.load(rf)
    objects = obj['xgboost_content']
    objects = postprocess_rules(objects)
    obj['rules_content'] = objects
    with open(pkl_path, 'wb') as wf:
        pickle.dump(obj, wf)
    return pkl_path
