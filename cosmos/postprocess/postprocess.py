from bs4 import BeautifulSoup
import re
import os
import glob
import codecs
from evaluate.evaluate import calculate_iou

def not_ocr(text):
    """
    TODO: Docstring for not_ocr.

    Args:
        text (TODO): TODO

    Returns: TODO

    """
    return 'ocr' not in text and 'rawtext' not in text and 'unicode' not in text

def construct_hocr_list(soup):
    hocr_list = []
    for seg_type in soup.find_all('div', not_ocr):
        hocr = seg_type.find_next('div', 'hocr')
        if hocr is not None:
            coordinates = hocr['data-coordinates']
            spl = coordinates.split(' ')
            int_coords = [int(x) for x in spl]
            hocr_list.append((seg_type, int_coords))
    return hocr_list


def check_caption_body(soup):
    """
    Check for things that look like captions that are flagged as body test.

    Simply looks for "figure", "fig", "table", or "tab" plus a number at the start of a line

    Args:
        soup (bs4.BeautiulSoup): The page's html, in soup form

    Returns: Corrected soup
    """
    
    # Basic rule to set Page headers and footers
    hocr_list = construct_hocr_list(soup)
    hocr_list = sorted(hocr_list, key=lambda x: x[1][1])
    ph = None
    if len(hocr_list) > 0:
        ph = hocr_list[0][1]

    for seg_type in soup.find_all('div', not_ocr):
        seg_class = " ".join(seg_type["class"])
        hocr = seg_type.find_next('div', 'hocr')
        #if hocr is not None:
        #    coordinates = hocr['data-coordinates']
        #    spl = coordinates.split(' ')
        #    spl = [int(x) for x in spl]
        #    if spl == ph:
        #        seg_type["class"] = "Page Header"
        lines = seg_type.find_all('span', 'ocr_line')
        if len(lines) > 0:
            for line in lines:
                clean_line = line.getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
                matches = re.findall('^(figure|fig)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
                if len(matches) >0:
                    seg_type["class"] = "Figure Caption"
                matches = re.findall('^(table|tbl|tab)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
                if len(matches) >0:
                    seg_type["class"] = "Table Caption"
    
    return soup

def check_overlap(obj_list, box, check_above_below=False, check_cls=None):
    for cls, bb, _ in obj_list:
        iou = calculate_iou(bb, box)
        if check_cls is not None and iou > 0 and cls == check_cls:
            continue
        if check_above_below:
            intersection = max(0, min(bb[2], box[2]) - max(bb[0], box[0]))
            if intersection == 0:
                return False
        if iou == 0:
            continue
        return False
    return True
 

def group_cls_columnwise(obj_list, g_cls):
    '''
    Given a list output from xml2list, group the class via columns
    :param obj_list: [(cls, coords, score)] list
    :return: Updated [(cls, coords, score)] list
    '''
    nbhds = []
    for obj in obj_list:
        cls, coords, _ = obj
        if cls == g_cls:
            if len(nbhds) == 0:
                nbhds.append(coords)
                continue
            new_nbhd_list = []
            added = False
            for nbhd_bb in nbhds:
                # construct a bounding box over this table and the neighborhood
                new_box = [min(nbhd_bb[0], coords[0]), min(nbhd_bb[1], coords[1]), max(nbhd_bb[2], coords[2]), max(nbhd_bb[3], coords[3])]
                if check_overlap(obj_list, new_box, check_above_below=True):
                    new_nbhd_list.append(new_box)
                    added = True
                else:
                    new_nbhd_list.append(nbhd_bb)
            # If we didn't merge with an existing neighborhood, create a new neighborhood
            if not added:
                new_nbhd_list.append(coords)
            nbhds = new_nbhd_list
    new_obj_list = []
    for obj in obj_list:
        cls, coords, _ = obj
        if cls != g_cls:
            new_obj_list.append(obj)
    for nbhd in nbhds:
        new_obj_list.append((g_cls, nbhd, 1))
    return new_obj_list

def group_cls(obj_list, g_cls):
    '''
    Given a list output from xml2list, group the class in that list
    :param obj_list: [(cls, coords, score)] list
    '''
    nbhds = []
    for obj in obj_list:
        cls, coords, _ = obj
        if cls == g_cls:
            if len(nbhds) == 0:
                nbhds.append(coords)
                continue
            new_nbhd_list = []
            added = False
            for nbhd_bb in nbhds:
                # construct a bounding box over this table and the neighborhood
                new_box = [min(nbhd_bb[0], coords[0]), min(nbhd_bb[1], coords[1]), max(nbhd_bb[2], coords[2]), max(nbhd_bb[3], coords[3])]
                if check_overlap(obj_list, new_box, check_cls=g_cls):
                    new_nbhd_list.append(new_box)
                    added = True
                else:
                    new_nbhd_list.append(nbhd_bb)
            # If we didn't merge with an existing neighborhood, create a new neighborhood
            if not added:
                new_nbhd_list.append(coords)
            nbhds = new_nbhd_list
    new_obj_list = []
    for obj in obj_list:
        cls, coords, _ = obj
        if cls != g_cls:
            new_obj_list.append(obj)
    for nbhd in nbhds:
        new_obj_list.append((g_cls, nbhd, 1))
    return new_obj_list


def postprocess(html_path, output_path):
    for f in glob.glob(os.path.join(html_path, "*.html")):
        with codecs.open(os.path.join(output_path, os.path.basename(f)), "w", "utf-8") as fout:
            with codecs.open(f, "r", "utf-8") as fin:
                soup = BeautifulSoup(fin, 'html.parser')
                new_soup = check_caption_body(soup)
                print(f"Writing to {output_path}")
                fout.write(str(new_soup))

