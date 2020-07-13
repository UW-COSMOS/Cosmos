"""
Run OCR over docs, also merge
"""

import functools
import json
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import pytesseract
from .group_cls import group_cls
import pandas as pd
from PIL import Image
import pickle
from ingest.process.detection.src.evaluate.evaluate import calculate_iou


def regroup(pkl_path):
    with open(pkl_path, 'rb') as rf:
        obj = pickle.load(rf)
    l = group_cls(obj['detected_objs'], 'Table', do_table_merge=True, merge_over_classes=['Figure', 'Section Header', 'Page Footer', 'Page Header'])
    obj['detected_objs'] = group_cls(l, 'Figure')
    with open(pkl_path, 'wb') as wf:
        pickle.dump(obj, wf)
    return pkl_path


def pool_text(pkl_path):
    with open(pkl_path, 'rb') as rf:
        obj = pickle.load(rf)
    meta_df = obj['meta']
    detect_objs = obj['detected_objs']
    if meta_df is not None:
        text_map = _pool_text_meta(meta_df, obj['dims'][3], detect_objs, obj['page_num'])
    else:
        text_map = _pool_text_ocr(obj['page_path'], detect_objs)
    obj['content'] = text_map
    with open(pkl_path, 'wb') as wf:
        pickle.dump(obj, wf)
    return pkl_path

def check_overlap(b2, row):
    b1 = (row['x1'], row['y1'], row['x2'], row['y2'])
    iou = calculate_iou(b1, b2)
    return iou != 0.0


def _pool_text_meta(meta_df, height, detect_objs, page_num):
    text_df = pd.DataFrame(meta_df)
    # Switch coordinate systems to bottom left is the origin
    text_df['y1'] = height - text_df['y1']
    text_df['y2'] = height - text_df['y2']
    text_df['y3'] = text_df['y1']
    text_df['y1'] = text_df['y2']
    text_df['y2'] = text_df['y3']
    pooled = []
    for dobj in detect_objs:
        bb, scrs = dobj
        bb = tuple(bb)
        tl_x, tl_y, br_x, br_y = bb
        # have to feather a bit, pdfs dont have tight bounding boxes usually
        tl_x -= 10
        br_x += 10
        tl_y -= 10
        br_y += 10
        go = functools.partial(check_overlap, (tl_x, tl_y, br_x, br_y))
        text_df['overlap'] = text_df.apply(go, axis=1)
        text_pool = text_df.loc[(text_df['page'] == page_num-1) & (text_df['overlap'] == True)]
        #text_pool = text_df.loc[(text_df['page'] == page_num-1) & (text_df['x2'] <= br_x) & (text_df['y1'] >= tl_y) &
        #                        (text_df['x1'] >= tl_x) & (text_df['y2'] <= br_y)]
        text_pool = text_pool.sort_values(by=['y2', 'x1'])
        text_pool = ' '.join(text_pool['text'].tolist())
        pooled.append((bb, scrs, text_pool))
    return pooled


def _pool_text_ocr(img_path, detect_objs):
    img = Image.open(img_path).convert('RGB')
    tess_df = pytesseract.image_to_data(img, output_type=pytesseract.Output.DATAFRAME)
    tess_df['bottom'] = tess_df['top'] + tess_df['height']
    tess_df['right'] = tess_df['left'] + tess_df['width']
    pooled = []
    for obj in detect_objs:
        bb, scrs = obj
        bb = tuple(bb)
        tl_x, tl_y, br_x, br_y = bb
        obj_ocr = tess_df.loc[(tess_df['bottom'] <= br_y) & (tess_df['top'] >= tl_y) &
                              (tess_df['left'] >= tl_x) & (tess_df['right'] <= br_x)]
        words = obj_ocr['text']
        word_list = []
        for ind, word in words.iteritems():
            word = str(word)
            if not word:
                continue
            word_list.append(word)
        word_list = [word for word in word_list if word != 'nan']
        word_dump = ' '.join(word_list)
        pooled.append((bb, scrs, word_dump))
    return pooled


def run(img, detect_objs):
    if len(detect_objs) == 0:
        return None, None
    l = group_cls(detect_objs, 'Table', do_table_merge=True, merge_over_classes=['Figure', 'Section Header', 'Page Footer', 'Page Header'])
    detect_objs = group_cls(l, 'Figure')
    width, height = img.size
    tess_df = pytesseract.image_to_data(img, output_type=pytesseract.Output.DATAFRAME)
    tess_df['bottom'] = tess_df['top'] + tess_df['height']
    tess_df['right'] = tess_df['left'] + tess_df['width']
    obj_str_list = []
    for obj in detect_objs:
        bb, scr_cls = obj
        tl_x, tl_y, br_x, br_y = bb
        obj_ocr = tess_df.loc[(tess_df['bottom'] <= br_y) & (tess_df['top'] >= tl_y) &
                          (tess_df['left'] >= tl_x) & (tess_df['right'] <= br_x)]
        words = obj_ocr['text']
        word_list = []
        for ind, word in words.iteritems():
            word = str(word)
            #word = strip_regx.sub('', word)
            if not word:
                continue
            word_list.append(word)
        word_list = [word for word in word_list if word != 'nan']
        word_dump = ' '.join(word_list)
        obj_str_list.append(word_dump)
    try:
        bbs, scrs = zip(*detect_objs)
    except Exception as e:
        logging.error(f'Detect_objs: {detect_objs}')
        raise e
    results = list(zip(bbs, scrs, obj_str_list))
    tess_df = tess_df.to_dict()
    tess_df = json.dumps(tess_df)
    tess_df = json.loads(tess_df)
    return tess_df, results

