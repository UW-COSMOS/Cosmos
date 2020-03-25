from ingest.process.detection.src.evaluate.evaluate import calculate_iou, match_lists
from ingest.process.detection.src.converters.xml2list import xml2list
import numpy as np
import os, glob
from bs4 import BeautifulSoup
import codecs
import re
import ast

expansion_delta = 50
orig_size=1920

def get_width_height(bbox):
    return bbox[2]-bbox[0], bbox[3]-bbox[1]

def compute_neighbors(center, predict_list):
    center_bbox,_,_ = center
    nbhds = []
    for cand in predict_list:
        if(center != cand):
            nbhd_bbox,_,_ = cand
            nbhd_bbox = [max(0, nbhd_bbox[0]-expansion_delta), max(0, nbhd_bbox[1]-expansion_delta), min(orig_size, nbhd_bbox[2]+expansion_delta), min(orig_size, nbhd_bbox[3]+expansion_delta)] 
            iou = calculate_iou(nbhd_bbox, center_bbox)
            if iou >0:
                nbhds.append([cand, iou])
    return nbhds

def compute_neighbors_train(center, predict_list):
    center_bbox,_,_,_,_ = center
    nbhds = []
    for cand in predict_list:
        if(center != cand):
            nbhd_bbox,_,_,_,_ = cand
            nbhd_bbox = [max(0, nbhd_bbox[0]-expansion_delta), max(0, nbhd_bbox[1]-expansion_delta), min(orig_size, nbhd_bbox[2]+expansion_delta), min(orig_size, nbhd_bbox[3]+expansion_delta)] 
            iou = calculate_iou(nbhd_bbox, center_bbox)
            if iou >0:
                nbhds.append([cand, iou])
    return nbhds


def not_ocr(text):
    """
    TODO: Docstring for not_ocr.

    Args:
        text (TODO): TODO

    Returns: TODO

    """
    return 'ocr' not in text and 'rawtext' not in text and 'unicode' not in text

def process_body(soup):
    """
    :return: [([x1, y1, x2, y2], [scores], [score], [seg_class], content)]
    """
    html_list = []
    for seg_type in soup.find_all('div', not_ocr):
        seg_class = " ".join(seg_type["class"])
        hocr = seg_type.find_next('div', 'hocr')
        if hocr is not None:
            coordinates = hocr['data-coordinates']
            spl = coordinates.split(' ')
            spl = [int(x) for x in spl]
            int_coords = [int(x) for x in spl]
            score = hocr['data-score']
            cls_scores = hocr['cls_scores']
        #    if spl == ph:
        #    seg_type["class"] = "Page Header"
            lines = seg_type.find_all('span', 'ocr_line')
            clean_line = ""
            if len(lines) > 0:
                line = lines[0]
                clean_line = line.getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
            html_list.append((int_coords, cls_scores, score, seg_class, clean_line.encode('utf-8').strip()))
    return html_list

def process_html(html_file):
    with codecs.open(html_file, "r", "utf-8") as fin:
        soup = BeautifulSoup(fin, 'html.parser')
        html_list = process_body(soup)    
        return html_list

def get_feat_vec(predict, predict_list, classes):
    max_nhds = 15
    feat_vec = []
    p_bb, p_cls_scores, text = predict
    p_score, p_cls = p_cls_scores[0]
    
    # Neighborhood features
    nbhds = compute_neighbors(predict, predict_list)
    nbhds_sorted = sorted(nbhds, key=lambda nbhds: (nbhds[0]))
    feat_nbhd1 = []
    nbhr_count = 1
    for nbhr in nbhds:
        nbhr_bb, nbhr_cls_scores, _ = nbhr[0]
        nbhr_score, nbhr_cls = nbhr_cls_scores[0]
        feat_nbhd1.append(classes.index(nbhr_cls))
        if nbhr_count == 15:
            break
        nbhr_count += 1
    feat_nbhd1.extend([-1] * (max_nhds - len(feat_nbhd1)))
    width, height = get_width_height(p_bb)  
        
    feat_vec.append(classes.index(p_cls))
    feat_vec.append(p_score)

    sorted_cls_scores = sorted(p_cls_scores, key=lambda ele: classes.index(ele[1])) # Predicted Confidence scores of all classes sorted based on classes
    sorted_scores = [row[0] for row in sorted_cls_scores]
    feat_vec.extend(sorted_scores)
    
    feat_vec.extend(p_bb) #Bounding box
    feat_vec.append(width) #Width of the object     
    feat_vec.append(height) #Height of the object
    feat_vec.append(width*height) #Area of the object
    feat_vec.extend(feat_nbhd1)
    
    #Textual features
    
    fig_matches = 1 if len(re.findall('^(figure|fig)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', text, flags=re.IGNORECASE|re.MULTILINE)) > 0 else 0
    table_matches = 1 if len(re.findall('^(table|tbl|tab)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', text, flags=re.IGNORECASE|re.MULTILINE)) > 0 else 0  
    feat_vec.append(fig_matches)
    feat_vec.append(table_matches)
   
    return feat_vec

def get_feat_vec_train(predict, predict_list, classes):
    max_nhds = 15
    feat_vec = []    
    p_bb, cls_scores, p_score, p_cls, text = predict
    
    # Neighbhorhood features
    nbhds = compute_neighbors_train(predict, predict_list)
    if len(nbhds) > max_nhds:
        max_nhds = len(nbhds)
    nbhds_sorted = sorted(nbhds, key=lambda nbhds: (nbhds[0]))
    feat_nbhd1 = []
    for nbhd in nbhds:
        nbhd_bb, cls_scores, nbhd_score, nbhd_cls, _ = nbhd[0]
        nbhd_score = 0.1
        nbhd_bb = tuple(nbhd_bb)
        feat_nbhd1.append(classes.index(nbhd_cls))
    feat_nbhd1.extend([-1] * (15 - len(feat_nbhd1)))

    width, height = get_width_height(p_bb)  
        
    feat_vec.append(classes.index(p_cls))
    feat_vec.append(p_score)

    feat_vec.extend(ast.literal_eval(cls_scores))
    
    feat_vec.extend(p_bb) #Bounding box
    feat_vec.append(width) #Width of the object     
    feat_vec.append(height) #Height of the object
    feat_vec.append(width*height) #Area of the object
    feat_vec.extend(feat_nbhd1)

    #Textual features
    
    fig_matches = 1 if len(re.findall('^(figure|fig)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', str(text), flags=re.IGNORECASE|re.MULTILINE)) > 0 else 0
    table_matches = 1 if len(re.findall('^(table|tbl|tab)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', str(text), flags=re.IGNORECASE|re.MULTILINE)) > 0 else 0 
    feat_vec.append(fig_matches)
    feat_vec.append(table_matches)   
   
    return feat_vec      


def get_target(predict, list_map, classes):
    p_bb,_ , p_score, p_cls,_ = predict
    p_score = 0.1
    p_bb = tuple(p_bb)
    matched_target = list_map[(p_cls, "", p_bb, p_score)]
    if matched_target is None:
        return -1
    gt_cls = matched_target[0][0]

    return classes.index(gt_cls)


def load_data_objs(predict_list, classes):
    features = []
    for predict in predict_list:
        features.append(get_feat_vec(predict, predict_list, classes))
    f = np.asarray(features)
    return f


def load_data_train(input_dir, classes):
    features = []
    targets = []
    for f in glob.glob(os.path.join(input_dir, "html/*.html")):
        predict_list = process_html(f)
        target_path = os.path.splitext(os.path.basename(f))[0]
        target_path = os.path.join(input_dir, "target/{}.xml".format(target_path))
        target_list = xml2list(target_path)

        list_map = match_lists(predict_list, target_list)
        for predict in predict_list:
            target = get_target(predict, list_map, classes)
            if target == -1:
                continue
            targets.append(target)
            features.append(get_feat_vec_train(predict, predict_list, classes))
    return np.array(features), np.array(targets)

#Use this function to load data for training the model
def load_data(input_dir, classes):
    features = []
    targets = []
    for f in glob.glob(os.path.join(input_dir, "html/*.html")):
        predict_list = process_html(f)
        target_path = os.path.splitext(os.path.basename(f))[0]
        target_path = os.path.join(input_dir, "target/{}.xml".format(target_path))
        target_list = xml2list(target_path)

        list_map = match_lists(predict_list, target_list)
        for predict in predict_list:
            target = get_target(predict, list_map, classes)
            if target == -1:
                continue
            targets.append(target)
            features.append(get_feat_vec_train(predict, predict_list, classes))
    return np.array(features), np.array(targets)

