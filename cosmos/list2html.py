"""
From a zipped list of class and coordinates, create an html file
"""

import dominate
from dominate.tags import *
import os
import pytesseract
from PIL import Image
import re
import xml.etree.ElementTree as ET
from dominate.util import raw
from latex_ocr.img2latex import img2latex_api, get_im2latex_model
from config import IM2LATEX_WEIGHT

im2latex_model = get_im2latex_model(IM2LATEX_WEIGHT)

def list2html(input_list, image_name, image_dir, output_dir, tesseract_hocr=True, tesseract_text=True, include_image=False):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        os.makedirs(os.path.join(output_dir, 'img'))
    doc = dominate.document(title=image_name[:-4])
    inter_path = os.path.join(output_dir, 'img', image_name[:-4])
    with doc:
        img = Image.open(os.path.join(image_dir, image_name))
        for ind, inp in enumerate(input_list):
            t, coords, score = inp
            cropped = img.crop(coords)
            input_id = str(t) + str(ind)
            hocr = pytesseract.image_to_pdf_or_hocr(cropped, extension='hocr').decode('utf-8')
            # Going to run a quick regex to find the body tag
            body = re.search(r'.*<body>(.*)</body>.*', hocr, re.DOTALL)
            b_text = body.group(1)
            d = div(id=input_id, cls=str(t))
            with d:
                if include_image:
                    if not os.path.exists(inter_path):
                        os.makedirs(inter_path)
                    output_img_path = os.path.join(output_dir, 'img')
                    if not os.path.exists(output_img_path):
                        os.makedirs(output_img_path)
                    crop_path = os.path.join(output_img_path, image_name[:-4],  f'{input_id}.png')
                    cropped.save(crop_path)
                    crop_img_path = os.path.join('img', image_name[:-4], f'{input_id}.png')
                    dominate.tags.img(src=crop_img_path)
                if b_text and tesseract_hocr:
                    # We do a quick loading and deloading to properly convert encodings
                    div(raw(b_text), cls='hocr', data_coordinates=f'{coords[0]} {coords[1]} {coords[2]} {coords[3]}')
                if tesseract_text:
                    if t == 'Equation':
                        txt = img2latex_api(im2latex_model, img=cropped, downsample_image_ratio=2, cropping=True, padding=True, gray_scale=True)
                    else:
                        txt = pytesseract.image_to_string(cropped, lang='eng')
                    div(txt, cls='rawtext')
    with open(os.path.join(output_dir, f'{image_name[:-4]}.html'), 'w', encoding='utf-8') as wf:
        wf.write(doc.render())
    
    
