import glob
from bs4 import BeautifulSoup
import os
import codecs
import torch
import torch.utils.data as data_utils
from utils.voc_utils import load_from_file
from xml2list import xml2list
import random
from evaluate.evaluate2 import calculate_iou
import ast
import numpy as np
import re

expansion_delta = 50
orig_size=1920

def get_width_height(bbox):
	return bbox[2]-bbox[0], bbox[3]-bbox[1]

def compute_neighbors(center, predict_list):
	_, _, center_bbox,_,_ = center
	nbhds = []
	for cand in predict_list:
		if(center != cand):
			_, _, nbhd_bbox,_,_ = cand
			nbhd_bbox = [max(0, nbhd_bbox[0]-expansion_delta), max(0, nbhd_bbox[1]-expansion_delta), min(orig_size, nbhd_bbox[2]+expansion_delta), min(orig_size, nbhd_bbox[3]+expansion_delta)] 
			iou = calculate_iou(nbhd_bbox, center_bbox)
			if iou >0:
				nbhds.append([cand, iou])
	return nbhds

def match_lists(predict_list, target_list):
	list_map = {}
	for prediction in predict_list:
		p_cls, cls_scores, p_bb, p_score,_ = prediction
		p_score = 0.1
		cls_scores = ""
		# make a tuple because i need to hash predicitons
		p_bb = tuple(p_bb)
		# Calculate which output annotation has the highest IOU
		ious = [calculate_iou(p_bb, target[2]) for target in target_list]
		if len(ious) == 0:
			list_map[(p_cls, cls_scores, p_bb, p_score)] = None
		else:
			max_iou = max(ious)
		if max_iou < 0.1:
			list_map[(p_cls, cls_scores, p_bb, p_score)] = None
			continue
		for ind, iou in enumerate(ious):
			if iou == max_iou:
				list_map[(p_cls, cls_scores, p_bb, p_score)] = (target_list[ind], iou)
				break
	return list_map

def not_ocr(text):
	"""
	TODO: Docstring for not_ocr.

	Args:
		text (TODO): TODO

	Returns: TODO

	"""
	return 'ocr' not in text and 'rawtext' not in text and 'unicode' not in text

def process_body(soup):
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
		#    if spl == ph:
		#	 seg_type["class"] = "Page Header"
			lines = seg_type.find_all('span', 'ocr_line')
			clean_line = ""
			if len(lines) > 0:
				line = lines[0]
				clean_line = line.getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
			#html_list.append((seg_class,"", int_coords, score, clean_line.encode('utf-8').strip()))
			html_list.append((seg_class,"", int_coords, score, clean_line))
	return html_list

def process_html(html_file):
	with codecs.open(html_file, "r", "utf-8") as fin:
		soup = BeautifulSoup(fin, 'html.parser')
		html_list = process_body(soup)	  
		return html_list

def featurize(input_dir, classes):
	class_distribution = dict((el,0) for el in classes)
	features = []
	targets = []
	max_nhds = 0
	no_match_count = 0
	for f in glob.glob(os.path.join(input_dir, "html*/*.html")):
		print(f)	
		html_list = process_html(f)
		target_path = os.path.splitext(os.path.basename(f))[0]
		target_path = os.path.join(input_dir, "target/{}.xml".format(target_path))
		target_list = xml2list(target_path)
		#print(target_list)
		predict_path = os.path.splitext(os.path.basename(f))[0]
		predict_path = os.path.join(input_dir, "xml/{}.xml".format(predict_path))
		predict_list = xml2list(predict_path)

		#list_map = match_lists(predict_list, target_list)
		list_map = match_lists(html_list, target_list)
		for predict in html_list:
			feat_vec = []
			p_cls, cls_scores, p_bb, p_score, text = predict

			#neighborhood features
			nbhds = compute_neighbors(predict, html_list)
			if len(nbhds) > max_nhds:
				 max_nhds = len(nbhds)
			nbhds_sorted = sorted(nbhds, key=lambda nbhds: (nbhds[0]))
			feat_nbhd1 = []
			feat_nbhd2 = []
			feat_nbhd3 = []
			for nbhd in nbhds:
				nbhd_cls, cls_scores, nbhd_bb, nbhd_score,_ = nbhd[0]
				nbhd_score = 0.1
				nbhd_bb = tuple(nbhd_bb)
				#feat_nbhd1.extend([-1] * (15 - len(feat_nbhd1)))
				feat_nbhd1.append(classes.index(nbhd_cls))
				feat_nbhd3.extend(cls_scores)
			feat_nbhd1.extend([-1] * (15 - len(feat_nbhd1)))
			#feat_nbhd2.extend([-1] * (15 * 4 - len(feat_nbhd2)))
			#feat_nbhd3.extend([0] * (15 * 11 - len(feat_nbhd3)))

			width, height = get_width_height(p_bb)	
			
			feat_vec.append(classes.index(p_cls))
			feat_vec.append(p_score)
			feat_vec.extend(cls_scores)
			feat_vec.extend(p_bb)
			feat_vec.append(width)
			feat_vec.append(height)
			feat_vec.append(width*height)
			feat_vec.append(p_bb[1])
			feat_vec.append(p_bb[3])
			feat_vec.extend(feat_nbhd1)
			#feat_vec.extend(feat_nbhd2)
			#feat_vec.extend(feat_nbhd3)

			#Textual features
			fig_matches = 1 if len(re.findall('^(figure|fig)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', text, flags=re.IGNORECASE|re.MULTILINE)) > 0 else 0
			table_matches = 1 if len(re.findall('^(table|tbl|tab)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', text, flags=re.IGNORECASE|re.MULTILINE)) > 0 else 0
			feat_vec.append(fig_matches)
			feat_vec.append(table_matches)	 

			p_score = 0.1
			p_bb = tuple(p_bb)
			matched_target = list_map[(p_cls, "", p_bb, p_score)]
			if matched_target is None:
				no_match_count += 1
				continue
			gt_cls = matched_target[0][0]
			features.append(np.array(feat_vec))
			targets.append(classes.index(gt_cls))
			
			class_distribution[gt_cls] += 1
	number_of_examples = len(features) 
	
	class_distribution = {k: v / number_of_examples for k, v in class_distribution.items()} 
	print("No_match_count : {} Maximum neighbors : {} Number of examples : {} Number of features : {} Number of classes : {}".format(no_match_count, max_nhds, len(features), len(features[0]), len(classes)))
	print("Class Distribution")
	print(class_distribution)
	return features, targets, len(features[0]), len(classes)


def get_dataset(input_dir, classes):
	features, targets, input_dim, output_dim  = featurize(input_dir, classes)
	features = np.array(features)
	targets = np.array(targets)
	return features, targets

