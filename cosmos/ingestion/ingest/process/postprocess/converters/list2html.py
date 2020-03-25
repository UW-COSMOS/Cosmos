"""
From a zipped list of class and coordinates, create an html file
"""

import dominate
from dominate.tags import *
import os
import pytesseract
from PIL import Image, ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True
import re
import string
from lxml import html, etree
from dominate.util import raw
from latex_ocr.img2latex import img2latex_api, get_im2latex_model
from config import IM2LATEX_WEIGHT
from .pdf_extractor import parse_pdf

BBOX_COORDINATES_PATTERN = re.compile("bbox\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)")
FILE_NAME_PATTERN = re.compile("(.*\.pdf)_([0-9]+)\.png")

with open('words_alpha.txt') as word_file:
    valid_words = set(word_file.read().split())
stop_words = ['all', 'am', 'an', 'and', 'any', 'are', 'as', 'at', 'be', 'but', 'by', 'can', 'did', 'do', 'few', \
'for', 'get', 'had', 'has', 'he', 'her', 'him', 'his', 'how', 'if', 'in', 'is', 'it', 'its', 'me', \
'my', 'nor', 'of', 'on', 'or', 'our', 'out', 'own', 'set', 'she', 'so', 'the', 'to', 'too', 'use', 'up', \
'was', 'we', 'who', 'why', 'you']

def get_coordinate(title):
    """
    Extract coordinates from ocrx coordinates string
    :param title: ocrx coordinate string
    :return: dictionary contains ``xmin``, ``ymin``, ``xmax``, ``ymax``
    """
    match = BBOX_COORDINATES_PATTERN.search(title)
    return {
        'xmin': int(match.group(1)),
        'ymin': int(match.group(2)),
        'xmax': int(match.group(3)),
        'ymax': int(match.group(4)),
    }

def variable_ocr(im2latex_model, root, sub_img, strip_tags):
    """
    Get the latex representation for each variable candidate.
    :param im2latex_model: Location of the im2latex model.
    :param root: Root of the etree.
    :param sub_img: Image of the section.
    :param strip_tags: Tags to be Tags to be flatten.
    :return: New tree.
    """
    #etree.strip_tags(root, *strip_tags)
    for word in root.xpath(".//*[@class='ocrx_word']"):
        if not word.text:
            continue
        text = word.text.strip()
        text = re.sub('['+string.punctuation+']', '', text)
        text = text.lower()
        if not text or text in stop_words:
            continue

        if not text in valid_words or len(text) <= 3:
            coord = get_coordinate(word.get('title'))
            sub_sub_img = sub_img.crop((coord['xmin'],coord['ymin'],coord['xmax'],coord['ymax']))
            output = img2latex_api(im2latex_model, img=sub_sub_img, downsample_image_ratio=2, cropping=True, padding=True, gray_scale=True)
            word.text = output
    return root

def coordinate_convert(x1,y1,x2,y2,max_of_x,max_of_y):
    """
    Convert the coordinate in the 1920 representation to the pdfminer representation.
    :param x1: Xmin of the 1920 representation.
    :param y1: Ymin of the 1920 representation.
    :param x2: Xmax of the 1920 representation.
    :param y2: Ymax of the 1920 representation.
    :param max_of_x: Maximum value of X in the pdfminer representation.
    :param max_of_y: Maximum value of Y in the pdfminer representation.
    :return: (Xmin,Ymin,Xmax,Ymax) in the pdfminer representation.
    """
    x_range = 1920*max_of_x/max_of_y
    xmin = x1/x_range*max_of_x
    xmax = x2/x_range*max_of_x
    ymin = (1920-y2)*max_of_y/1920
    ymax = (1920-y1)*max_of_y/1920
    return (xmin,ymin,xmax,ymax)

def valid_xml_char_ordinal(c):
    """
    Check if a character is valid in xml.
    :param c: Input character.
    :return: True or False.
    """
    codepoint = ord(c)
    # conditions ordered by presumed frequency
    return (
        0x20 <= codepoint <= 0xD7FF or
        codepoint in (0x9, 0xA, 0xD) or
        0xE000 <= codepoint <= 0xFFFD or
        0x10000 <= codepoint <= 0x10FFFF
        )

def invalid_filter(s):
    """
    Remove invalid characters from a string.
    :param s: Input string.
    :return: String after filter.
    """
    out = ''
    for c in s:
        if valid_xml_char_ordinal(c):
            out += c
    return out

def unicode_representation(unicode_df, page, root, base, t):
    """
    Get the unicode representation for each section.
    :param unicode_df: Dataframe containing unicode for each token.
    :param page: Page of the section.
    :param root: Root of the section tree.
    :param base: Base of the coordinate.
    :param t: Type of the section.
    :return: New tree containing the unicode representation. 
    """
    df = unicode_df[0]
    limit = unicode_df[1]
    #The file doesn't have unicode
    if df is None:
        return root, 'Have no Unicode', -1
    MAX_OF_X = limit[2]
    MAX_OF_Y = limit[3]
    df_page = df[df['page'] == page-1]
    first_id = -1
    first = True
    num_skip = 0 
    for word in root.xpath(".//*[@class='ocrx_word']"):
        coord = get_coordinate(word.get('title'))
        coordinate = coordinate_convert(coord['xmin']+base[0],coord['ymin']+base[1],coord['xmax']+base[0],coord['ymax']+base[1],MAX_OF_X,MAX_OF_Y)
        paddy = (coordinate[3]-coordinate[1])*0.3
        #print('converted coord: '+str(coordinate[0])+' '+str(coordinate[1])+' '+str(coordinate[2])+' '+str(coordinate[3]))
        index = ~( (df_page['x1'] >= coordinate[2]) | (df_page['x2'] <= coordinate[0]) | \
                (df_page['y1'] >= coordinate[3]-paddy) | (df_page['y2'] <= coordinate[1]+paddy) )
        df_within = df_page[index]
        text = ''
        idx_prev = -1
        for idx, row in df_within.iterrows():            
            if first:
                first_id = idx
                first = False
            if idx_prev == -1:
                text += invalid_filter(row['text'])
                text += ' '
                idx_prev = idx
            else:
                if idx == idx_prev+1:
                    text += invalid_filter(row['text'])
                    text += ' '
                    idx_prev = idx
                else:
                    num_skip += 1

            #print('coord within: '+str(row['x1'])+' '+str(row['y1'])+' '+str(row['x2'])+' '+str(row['y2']))
        word.text = text

    #print('Number of skip: '+str(num_skip))

    if t == 'Equation':
        coordinate = coordinate_convert(base[0],base[1],base[2],base[3],MAX_OF_X,MAX_OF_Y)
        paddy = (coordinate[3]-coordinate[1])*0.0
        index = ~( (df_page['x1'] >= coordinate[2]) | (df_page['x2'] <= coordinate[0]) | \
                (df_page['y1'] >= coordinate[3]-paddy) | (df_page['y2'] <= coordinate[1]+paddy) )
        df_within = df_page[index]
        text = ''
        for idx, row in df_within.iterrows():
            text += row['text']
            text += ' '
    else:
        text = ' '
    return root,text,first_id
        

def list2html(input_list, image_name, image_dir, output_dir, unicode_df=None,tesseract_hocr=True, tesseract_text=True, include_image=True, feather_x=2, feather_y=2):
    """
    Given an input list, write a corresponding html file. All extractions and postprocessing occur in this function.
    :param input_list: List output from xml2list
    :param image_name: Name of image corresponding to page in pdf
    :param image_dir: image directory
    :param output_dir: output directory to write html
    :param unicode_df: Optional dataframe containing unicode information
    :param tesseract_hocr: flag to include tesseract hocr
    :param tesseract_text: flag to include tesseract text
    :param include_image: flag to include cropped images for each hocr
    :param feather_x: x feathering parameter to increase accuracy of ocr
    :param feather_y: x feathering parameter to increase accuracy of ocr
    """
    doc = dominate.document(title=image_name[:-4])
    
    inter_path = os.path.join(output_dir, 'img', image_name[:-4])
    #im2latex_model = get_im2latex_model(IM2LATEX_WEIGHT)
    with doc:
        img = Image.open(os.path.join(image_dir, image_name))
        for ind, inp in enumerate(input_list):
            t, coords, score = inp
            width, height = img.size
            # Feather the coords here a bit so we can get better OCR
            ccoords = [max(coords[0]-feather_x, 0), max(coords[1]-feather_y, 0),
                          min(coords[2]+feather_x, width), min(coords[3]+feather_y, height)]
            cropped = img.crop(ccoords)
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
                    div(raw(b_text), cls='hocr', data_coordinates=f'{coords[0]} {coords[1]} {coords[2]} {coords[3]}', data_score=f'{score}')
                    loaded = html.fromstring(b_text)                    
                    # Running variable ocr is too expensive right now. We need a better solution for this
                    # If we were to run variable ocr over every token, it would go here.
                    tree = etree.fromstring(etree.tostring(loaded))
                    if unicode_df is not None:
                        match = FILE_NAME_PATTERN.search(image_name)
                        pdf_name = '/input/'+match.group(1)
                        page_num = int(match.group(2))
                        unicode_tree,text,first_id = unicode_representation(unicode_df, page_num, tree, coords, t)
                        #occasionally the class here would be replaced by 'Page Header', cannot figure our why
                        div(raw(etree.tostring(unicode_tree).decode("utf-8")), cls='text_unicode', data_coordinates=f'{coords[0]} {coords[1]} {coords[2]} {coords[3]}', id=str(first_id))
                        div(text, cls='equation_unicode')  


    with open(os.path.join(output_dir, f'{image_name[:-4]}.html'), 'w', encoding='utf-8') as wf:
        wf.write(doc.render())

    
    
    
