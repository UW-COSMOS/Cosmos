
"""
Route to handle decomposition of pages into page objects
"""
import os
from PIL import Image
from ingest.utils.visualize import write_regions
from ingest.utils.normalize_text import normalize_text
from ingest.process.proposals.connected_components import get_proposals
from ingest.process.aggregation.aggregate import aggregate_router
from ingest.process.detection.src.preprocess import pad_image
from ingest.process.postprocess.xgboost_model.inference import run_inference as postprocess
from ingest.process.postprocess.pp_rules import apply_rules as postprocess_rules
import pandas as pd
from dask.distributed import get_worker
import pickle
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.INFO)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logging.getLogger("PIL").setLevel(logging.WARNING)
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

def aggregate(pck, aggregations, result_path, images_path, pdfname):
    pck = [i for j in pck for i in j]
    logger.info(f"Aggregating {len(pck)} objects")
    result_df = pd.DataFrame(pck)
    result_df['detect_cls'] = result_df['classes'].apply(lambda x: x[0])
    result_df['detect_score'] = result_df['scores'].apply(lambda x: x[0])

    for aggregation in aggregations:
        aggregate_df = aggregate_router(result_df, aggregate_type=aggregation, write_images_pth=images_path)
        pdfname = os.path.basename(pdfname)
        name = f'{pdfname}_{aggregation}.parquet'
        aggregate_df.to_parquet(os.path.join(result_path, name), engine='pyarrow', compression='gzip')
    # TODO: this isn't really necessary probably? We're not transforming pck in this function, we're just sending it back to make the workflow management slightly easier to read.
    return result_df

def get_objects(pck, use_text_normalization):
    final_objects = []
    with open(pck, 'rb') as rf:
        final_obj = {}
        obj = pickle.load(rf)
        for ind, c in enumerate(obj['content']):
            bb, cls, text = c
            if use_text_normalization:
                text = normalize_text(text)
            scores, classes = zip(*cls)
            scores = list(scores)
            classes = list(classes)
            postprocess_cls = postprocess_score = None
            if 'xgboost_content' in obj:
                _, postprocess_cls, _, postprocess_score = obj['xgboost_content'][ind]
                if 'rules_content' in obj:
                    _, postprocess_cls, _, postprocess_score = obj['rules_content'][ind]
            final_obj = {'pdf_name': obj['pdf_name'],
                         'dataset_id': obj['dataset_id'],
                         'page_num': obj['page_num'],
                         'img_pth': obj['pad_img'],
                         'pdf_dims': list(obj['pdf_limit']),
                         'bounding_box': list(bb),
                         'classes': classes,
                         'scores': scores,
                         'content': text,
                         'postprocess_cls': postprocess_cls,
                         'postprocess_score': postprocess_score
                        }
            final_objects.append(final_obj)
    return final_objects

def xgboost_postprocess(pkl_path):
    with open(pkl_path, 'rb') as rf:
        obj = pickle.load(rf)
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
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
