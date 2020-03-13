"""
From a zipped list of class and coordinates, create json file
"""

import os
from PIL import Image, ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True
import re

def list2obj(input_list, img_path):
    result = {}
    img = Image.open(img_path)
    width, height = img.size
    result['img_width'] = width
    result['img_height'] = height
    with open(img_path, 'rb') as rf:
        result['img_bytes'] = rf.read()
    m = re.match(r"(.*)_(.*)\..*", image_name)
    if not m or not m[1] or not m[2]:
        raise Exception('Invalid image name provided to list2json')
    result['doc_name'] = m[1]
    result['page_number'] = int(m[2])
    obj_list = [{'cls': i[0], 'coordinates': i[1], 'score': i[2]} for i in input_list]
    result['page_objects'] = obj_list
    return result


    

    
    
    
