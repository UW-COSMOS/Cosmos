"""
From a zipped list of class and coordinates, create an html file
"""

import dominate
from dominate.tags import *
import os
import pytesseract
from PIL import Image


def list2html(input_list, image_name, image_dir, output_dir, hocr_f=True):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        os.makedirs(os.path.join(output_dir, 'img'))
    doc = dominate.document(title=image_name[:-4])
    img = Image.open(os.path.join(image_dir, image_name))
    inter_path = os.path.join(output_dir, 'img', image_name[:-4])
    with doc.head:
        script(src='https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js')
        script("$(function(){var includes = $('[data-include]');jQuery.each(includes, function(){var file = 'hocr/' + $(this).data('include') + '.html';$(this).load(file);});});")
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
            output_hocr_path = os.path.join(output_dir, 'hocr')
            if not os.path.exists(output_hocr_path):
                os.makedirs(output_hocr_path)
            hocr_specific_path = os.path.join(output_hocr_path, image_name[:-4])
            if not os.path.exists(hocr_specific_path):
                os.makedirs(hocr_specific_path)
            crop_path = os.path.join(output_img_path, image_name[:-4],  f'{input_id}.png')
            cropped.save(crop_path)
            txt = None
            hocr = None
            if hocr_f:
                hocr = pytesseract.image_to_pdf_or_hocr(cropped, extension='hocr')
            else:
                txt = pytesseract.image_to_string(cropped, lang='eng')
            if hocr_f:
                with open(os.path.join(hocr_specific_path, f'{input_id}.html'), 'wb') as wf:
                    wf.write(hocr)
            d = div(id=input_id, cls=str(t))
            crop_img_path = os.path.join('img', image_name[:-4], f'{input_id}.png')
            with d:
                dominate.tags.img(src=crop_img_path)
                if hocr_f:
                    div(data_include=f'{image_name[:-4]}/{input_id}')
                else:
                    div(txt)
    with open(os.path.join(output_dir, f'{image_name[:-4]}.html'), 'w') as wf:
        wf.write(doc.render())
    
    
