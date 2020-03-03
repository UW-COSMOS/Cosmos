"""
Run OCR over docs, also merge
"""

import json
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import time
import io
from PIL import Image
import re
import pytesseract
from .group_cls import group_cls
import pandas as pd

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

