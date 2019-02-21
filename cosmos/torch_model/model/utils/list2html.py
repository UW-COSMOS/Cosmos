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


def list2html(input_list, image_name, image_dir, output_dir):
    doc = dominate.document(title=image_name[:-4])
    img = Image.open(os.path.join(image_dir, image_name))
    inter_path = os.path.join(output_dir, 'img', image_name[:-4])
    with doc:
        for ind, inp in enumerate(input_list):
            t, coords = inp
            input_id = str(t) + str(ind)
            cropped = img.crop(coords)
            if not os.path.exists(inter_path):
                os.makedirs(inter_path)
            output_img_path = os.path.join(output_dir, 'img')
            if not os.path.exists(output_img_path):
                os.makedirs(output_img_path)
            crop_path = os.path.join(output_img_path, image_name[:-4],  f'{input_id}.png')
            cropped.save(crop_path)
            hocr = pytesseract.image_to_pdf_or_hocr(cropped, extension='hocr').decode('utf-8')
            # Going to run a quick regex to find the body tag
            body = re.search(r'.*<body>(.*)</body>.*', hocr, re.DOTALL)
            b_text = body.group(1)
            txt = pytesseract.image_to_string(cropped, lang='eng')
            d = div(id=input_id, cls=str(t))
            crop_img_path = os.path.join('img', image_name[:-4], f'{input_id}.png')
            with d:
                dominate.tags.img(src=crop_img_path)
                if b_text:
                    # We do a quick loading and deloading to properly convert encodings
                    div(raw(b_text), id='hocr', data_coordinates=f'{coords[0]} {coords[1]} {coords[2]} {coords[3]}')
                div(txt, id='rawtext')
    with open(os.path.join(output_dir, f'{image_name[:-4]}.html'), 'w', encoding='utf-8') as wf:
        wf.write(doc.render())
    
