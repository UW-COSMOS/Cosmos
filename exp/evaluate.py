import os
from xml2list import xml2list
from mrcnn.config import Config
from PIL import Image, ImageDraw
from mrcnn import utils
import mrcnn.model as modellib
from mrcnn import visualize
from mrcnn.model import log
from dataset import PageDataset
import numpy as np
from mrcnn import visualize
from tqdm import tqdm
from config import PageConfig
from random import sample
from voc_utils import ICDAR_convert, similar_class_sets
from model2xml import model2xml
import matplotlib.pyplot as plt

def run_past_eval():
    MODEL_DIR = "/users/ankurgos/MaskMount/Mask-RCNN-exp/exp/weights"
    class InferenceConfig(Config):
        NAME = "pages_uncollapsed"
        BACKBONE = "resnet50"
        GPU_COUNT = 1
        IMAGE_MAX_DIM = 1920
        RPN_ANCHOR_SCALES = (32,64, 256, 512,1024)
        NUM_CLASSES = 16
        IMAGES_PER_GPU = 1
    def draw_image(name, path, rois, classes):
        im = Image.open(path)
        d = ImageDraw.Draw(im)
        for idx, roi in enumerate(rois):
                y1, x1, y2, x2 = roi
                d.text((x1+5,y1+5), classes[idx], fill="#0f0")
                points = (x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)
                for i, pt in enumerate(points[:-1]):
                        d.line((pt, points[i+1]), fill="#f00", width=5)
        im.save(f"{name}.png", "png")
    
    inference_config = InferenceConfig()
    config = PageConfig()
    model = modellib.MaskRCNN(mode="inference", 
                              config=inference_config,
                              model_dir=MODEL_DIR)
    
    model_path = model.find_last()
    
    print("Loading weights from ", model_path)
    model.load_weights(model_path, by_name=True)
    data_test = PageDataset('test', '/users/ankurgos/MaskMount/Mask-RCNN-exp/exp/data', 0)
    data_test.load_page(classes=list(ICDAR_convert.keys()))
    data_test.prepare()
    image_ids = data_test.image_ids
    APs = dict([(cls, []) for cls in data_test.class_names])
    ious = dict([(cls, []) for cls in data_test.class_names])
    
    for idx, image_id in enumerate(tqdm(image_ids)): 
        # Load image and ground truth data
        image, image_meta, gt_class_id, gt_bbox, gt_mask =\
            modellib.load_image_gt(data_test, inference_config,
                                        image_id, use_mini_mask=False)
        results = model.detect([image], verbose=0)
        info = data_test.image_info[image_id]
        r = results[0]
        zipped = zip(r['class_ids'], r['rois'])
        model2xml(info["str_id"], 'predictions', [1920, 1920], zipped, data_test.class_names, r['scores'])
#        for idx, cls in enumerate(data_test.class_names[1:]):
#                cls_idx = idx+1
#                mask = [cls == cls_idx for cls in gt_class_id]
#                testing_bbox = gt_bbox[mask]
#                testing_id = gt_class_id[mask]
#                if len(testing_id)==0:
#                        continue
#                testing_mask = gt_mask[:,:,mask]
#                # Compute AP
#                AP,precisions, recalls, overlaps =\
#                utils.compute_ap(testing_bbox, testing_id, testing_mask,
#                             r["rois"], r["class_ids"], r["scores"], r['masks'])
#                APs[cls].append(AP)
#                ious[cls].extend(np.amax(overlaps, axis=0, initial=0))
#    
#    for AP_key in APs.keys():
#        print(f"{AP_key} AP: {np.mean(APs[AP_key])}")
#        print(f"{AP_key} IoU: {np.mean(ious[AP_key])}")

def calculate_iou(box1, box2):
    # Shamelessly adapted from
    # https://stackoverflow.com/questions/25349178/calculating-percentage-of-bounding-box-overlap-for-image-detector-evaluation
    # determine the coordinates of the intersection rectangle
    x_left = max(box1[0], box2[0])
    y_top = max(box1[1], box2[1])
    x_right = min(box1[2], box2[2])
    y_bottom = min(box1[3], box2[3])

    if x_right < x_left or y_bottom < y_top:
        return 0.0

    # The intersection of two axis-aligned bounding boxes is always an
    # axis-aligned bounding box
    intersection_area = (x_right - x_left) * (y_bottom - y_top)

    # compute the area of both AABBs
    bb1_area = (box1[2] - box1[0]) * (box1[3] - box1[1])
    bb2_area = (box2[2] - box2[0]) * (box2[3] - box2[1])

    # compute the intersection over union by taking the intersection
    # area and dividing it by the sum of prediction + ground-truth
    # areas - the interesection area
    iou = intersection_area / float(bb1_area + bb2_area - intersection_area)
    return iou


def match_lists(predict_list, target_list):
    list_map = {}
    for prediction in predict_list:
        p_cls, p_bb, p_score = prediction
        # make a tuple because i need to hash predicitons
        p_bb = tuple(p_bb)
        # Calculate which output annotation has the highest IOU
        ious = [calculate_iou(p_bb, target[1]) for target in target_list]
        if len(ious) == 0:
            list_map[(p_cls, p_bb, p_score)] = None
        else:
            max_iou = max(ious)
            if max_iou < 0.1:
                list_map[(p_cls, p_bb, p_score)] = None
                continue
            for ind, iou in enumerate(ious):
                if iou == max_iou:
                    list_map[(p_cls, p_bb, p_score)] = (target_list[ind], iou)
                    break
    return list_map



def run_evaluate(predict_dir, target_dir):
    fp_list = []
    tp_num = 0
    for predict_f in os.listdir(predict_dir):
        predict_path = os.path.join(predict_dir, predict_f)
        target_path = os.path.join(target_dir, predict_f)
        predict_list = xml2list(predict_path)
        target_list = xml2list(target_path)
        list_map = match_lists(predict_list, target_list)
        for predict in predict_list:
            p_cls, p_bb, p_score = predict
            p_bb = tuple(p_bb)
            matched_target = list_map[(p_cls, p_bb, p_score)]
            if matched_target is None:
                fp_list.append((predict, 'background'))
                continue
            t, iou = matched_target
            t_cls, t_bb, t_score = t
            if p_cls == t_cls:
                if iou < 0.5:
                    fp_list.append((predict, 'localization'))
                    continue
                tp_num += 1
            else:
                sim = False
                for s in similar_class_sets:
                    if p_cls in s and t_cls in s:
                        sim = True
                        break
                if sim:
                    fp_list.append((predict, 'similar'))
                else:
                    fp_list.append((predict, 'other'))
    fp_list.sort(key=lambda x: x[0][2])
    print(f'True Positives: {tp_num}')
    return fp_list

def calculate_statistics_map(fp_list):
    statistics = {
        'background': 0,
        'localization': 0,
        'similar': 0,
        'other': 0
    }
    stats_map = {'all': statistics.copy()}
    for fp in fp_list:
        p, fp_type = fp
        p_cls, p_bb, p_score = p
        if p_cls not in stats_map:
            stats_map[p_cls] = statistics.copy()
        stats_map['all'][fp_type] += 1
        stats_map[p_cls][fp_type] += 1
    return stats_map


def make_pie_charts(stats_map):
    chart_num = 0
    for stat_key in stats_map:
        stats = stats_map[stat_key]
        fp_types = stats.keys()
        fig1, ax1 = plt.subplots()
        ax1.set_title(f'False positive distribution: {stat_key}')
        ax1.pie(stats.values(), labels=fp_types, autopct='%1.1f%%')
        ax1.axis('equal')
        plt.savefig(f'charts/pie{chart_num}.png')
        chart_num += 1


if __name__ == '__main__':
    fp_list = run_evaluate('predictions/', 'data/annotations')
    smap = calculate_statistics_map(fp_list)
    make_pie_charts(smap)
    


