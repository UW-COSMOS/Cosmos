
import os
from ingest.process.detection.src.converters.xml2list import xml2list
from PIL import Image, ImageDraw
import numpy as np
from tqdm import tqdm
from random import sample
import matplotlib.pyplot as plt
from ingest.process.detection.src.utils.voc_utils import similar_class_sets, ICDAR_convert
plt.style.use('ggplot')
import pandas as pd
import click


def rect(d, im, color, points):
        for i, pt in enumerate(points[:-1]):
                d.line((pt, points[i+1]), fill=color, width=4)

def draw_roi(d, im, color, rois, cls, ks, down=False, scores=None):
    y1, x1, y2, x2 = roi
    #points = (x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)
    # Replace the string with some valid font
    d.rectangle([x1, y1, x2, y2], outline=color)
    #rect(d, im, color, points )



def draw_image(name, path, rois, gt_rois, gt_classes, classes, scores):
        im = Image.open(path)
        d = ImageDraw.Draw(im)
        draw_rois(d, im, "#f00", rois, classes, scores=scores)
        draw_rois(d, im, "#00f", gt_rois, gt_classes, down=True)
        im.save(f"result_collapse/{name}.png", "png")
        plt.figure(figsize=(20, 20))
        plt.imshow(np.asarray(im), aspect='auto')

def calculate_iou(box1, box2, contains=False):
    # Shamelessly adapted from
    # https://stackoverflow.com/questions/25349178/calculating-percentage-of-bounding-box-overlap-for-image-detector-evaluation
    # determine the coordinates of the intersection rectangle
    """
    Calculate the IoU of two boxes
    :param box1: (tl_x, tl_y, br_x, br_y) box 1
    :param box2: (tl_x, tl_y, br_x, br_y) box 1
    :return: IoU overlap
    """
    x_left = max(box1[0], box2[0])
    y_top = max(box1[1], box2[1])
    x_right = min(box1[2], box2[2])
    y_bottom = min(box1[3], box2[3])
    
    if contains:
        if box2[0] <= box1[0] and box2[1] <= box1[1] and box2[2] >= box1[2] and box2[3] >= box1[3]:
            return 1.0

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
        p_cls, p_bb, p_score= prediction
        p_score = 0.1
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


color_classes =  {"Section Header":"#800000", "Body Text":"#e6194B", 
                  "Figure":"#e6194B", "Figure Caption":"#ffe119", "Table":"#bfef45", 
                  "Equation":"#3cb44b", "Page Footer":"#42d4f4",
                  "Page Header":"#4363d8", "Table Caption":"#911eb4", 
                  "Table Note":"#f032e6", "Abstract":"#a9a9a9", "Other":"#469990", 
                  "Equation label":"#aaffc3", "Reference text":"#9A6324", "Figure Note":"#ffd8b1"}

def run_evaluate(predict_dir, target_dir, output_dir, img_dir=None, simi=False, thres=0):
    fp_list = []
    classification_p_list = []
    total_intersection = 0
    total_prediction = 0
    total_gt = 0
    for predict_f in os.listdir(predict_dir):
        predict_path = os.path.join(predict_dir, predict_f)
        target_path = os.path.join(target_dir, predict_f)
        predict_list = xml2list(predict_path)
        target_list = xml2list(target_path)
        if img_dir is not None:
            img_p = os.path.join(img_dir, predict_f[:-4] + '.png')
            img = Image.open(img_p)
            for predict in predict_list:
                p_cls, p_bb, p_score = predict
                p_bb = [x - 5 for x in p_bb]
                d = ImageDraw.Draw(img)
                d.rectangle(p_bb, outline=color_classes[p_cls])
                img.save(os.path.join(output_dir, f'{predict_f[:-4] + ".png"}'))

        list_map = match_lists(predict_list, target_list)
        tbb_map = {}
        for predict in predict_list:
            p_cls, p_bb, p_score = predict
            p_score = 0.1
            p_bb = tuple(p_bb)
            matched_target = list_map[(p_cls, p_bb, p_score)]
            if matched_target is None:
                fp_list.append((predict, 'background'))
                continue
            t, iou = matched_target
            t_cls, t_bb, t_score = t
            t_bb = tuple(t_bb)
            #t_cls = ICDAR_convert[t_cls]
            if t_bb in tbb_map:
                tbb_map[t_bb].append(p_bb)
            else:
                tbb_map[t_bb] = [p_bb]
            if p_cls == t_cls:
                if iou < thres:
                    fp_list.append((predict, 'localization'))
                    continue
                fp_list.append((predict, 'correct'))
                classification_p_list.append((p_cls, t_cls))
                continue
            else:
                if iou >= thres:
                    classification_p_list.append((p_cls, t_cls))
                sim = False
                for s in similar_class_sets:
                    if p_cls in s and t_cls in s:
                        sim = True
                        break
                if sim:
                    if simi:
                        fp_list.append((predict, 'correct'))
                    else:
                        fp_list.append((predict, 'similar'))
                else:
                    fp_list.append((predict, 'other'))
        page_intersection = 0
        page_prediction = 0
        page_gt = 0
        for t_bb in tbb_map:
            for prediction in tbb_map[t_bb]:
                x_left = max(t_bb[0], prediction[0])
                y_top = max(t_bb[1], prediction[1])
                x_right = min(t_bb[2], prediction[2])
                y_bottom = min(t_bb[3], prediction[3])
                intersection_area = (x_right - x_left) * (y_bottom - y_top)
                page_intersection += intersection_area
                page_prediction += (prediction[2] - prediction[0]) * (prediction[3] - prediction[1])
            page_gt += (t_bb[2] - t_bb[0]) * (t_bb[3] - t_bb[1])
        total_intersection += page_intersection
        total_prediction += page_prediction
        total_gt += page_gt

    print('Bounding box Precision')
    bb_precision = total_intersection / total_prediction
    print(bb_precision)
    print('--------')
    print('Bounding box Recall')
    bb_recall = total_intersection / total_gt
    print(bb_recall)
    print('--------')
    print('Bounding box F1')
    bb_f1 = 2 * bb_precision * bb_recall / (bb_precision + bb_recall)
    print(bb_f1)
    print('---------')

    class_counts = {}
    for p in classification_p_list:
        p_cls, t_cls = p
        if p_cls not in class_counts:
            class_counts[p_cls] = {}
        if t_cls in class_counts[p_cls]:
            class_counts[p_cls][t_cls] += 1
        else:
            class_counts[p_cls][t_cls] = 1
    class_precisions = {}
    all_tp = 0
    all_denom = 0
    for p_cls in class_counts:
        tp = 0
        fp = 0
        for t_cls in class_counts[p_cls]:
            if p_cls == t_cls:
                tp = class_counts[p_cls][t_cls]
            else:
                fp += class_counts[p_cls][t_cls]
        denom = tp + fp
        all_tp += tp
        all_denom += denom
        class_precisions[p_cls] = tp / denom if denom != 0 else 'No false positives or true positives found'
    print('All class precision')
    all_precision = all_tp / all_denom
    print(all_precision)
    print('-----------------')
    all_tp = 0
    all_denom = 0
    class_recalls = {}
    print('DEBUG')
    for p_cls in class_counts:
        print(class_counts)
        tp = class_counts[p_cls][p_cls] if p_cls in class_counts[p_cls] else 0
        fn = 0
        for p2_cls in class_counts:
            if p2_cls == p_cls:
                continue
            if p_cls in class_counts[p2_cls]:
                fn += class_counts[p2_cls][p_cls]
        denom = tp + fn
        all_tp += tp
        all_denom += denom
        class_recalls[p_cls] = tp / denom if denom != 0 else 'No false negatives or true positives found'

    print('All class recall')
    all_recall = all_tp / all_denom
    print(all_recall)
    print('--------------')

    print('All class F1')
    all_f1 = 2 * all_precision * all_recall / (all_precision + all_recall)
    print(all_f1)
    print('--------------')

    print('Class recalls')
    print(class_recalls)
    print('------------')
    print('Class precisions')
    print(class_precisions)
    print('------------')

    class_f1 = {}
    for cl in class_recalls:
        rec = class_recalls[cl]
        prec = class_precisions[cl]
        if type(rec) == str:
            print(f'Class: {cl}')
            print(rec)
            continue
        if rec + prec == 0:
            class_f1[cl] = 0
            continue
        class_f1[cl] = 2 * rec * prec / (rec + prec)
    print('Class F1s')
    print(class_f1)
    print('-------------')


    print('Class counts')

    print(class_counts)
    df = pd.DataFrame(class_counts)
    df = df.fillna(value=0)
    df['Total'] = df.sum(axis=1)
    print(df[sorted(df.columns)])
    print('------------')


    tp_num = 0
    fp_num = 0
    current_class = None
    roc_tp = [0]
    roc_fp = [0]
    p_r_curve = []
    for p in fp_list:
        predict, category = p
        is_tp = category == 'correct'
        if is_tp:
            if current_class is None:
                current_class = True
                continue
            if not current_class:
                roc_tp.append(tp_num)
                roc_fp.append(fp_num)
            tp_num += 1
        else:
            if current_class is None:
                current_class = False
                continue
            if current_class:
                roc_tp.append(tp_num)
                roc_fp.append(fp_num)
            fp_num += 1
        precision = tp_num / (tp_num + fp_num)
        p_r_curve.append((precision, tp_num))
    roc_tp.append(tp_num)
    roc_fp.append(fp_num)
    p_r_curve = [(x, y / tp_num) for x, y in p_r_curve]
    max_ps = []
    for i in range(11):
        chk_num = i / 10
        m_p = 0
        for x, y in p_r_curve:
            if y <= chk_num:
                continue
            if x > m_p:
                m_p = x
        max_ps.append(m_p)
    mAP = sum(max_ps) / len(max_ps)
            

    uz = list(zip(*p_r_curve))
    make_p_r_curve(uz[0], uz[1], output_dir)
    normalized_tp = [x / tp_num for x in roc_tp]
    if fp_num > 0:
        normalized_fp = [x / fp_num for x in roc_fp]
        make_roc_chart(normalized_tp, normalized_fp, output_dir)

    filtered_fp_list = [fp for fp in fp_list if fp[1] != 'correct']
    print(f'True Positives: {tp_num}')
    print(f'False Positives: {fp_num}')
    return filtered_fp_list

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


def make_p_r_curve(precisions_list, recalls_list, output_dir):
    fig1, ax1 = plt.subplots()
    ax1.plot(recalls_list, precisions_list)
    ax1.axis([0, 1, 0, 1])
    ax1.set_title('Precision-Recall Curve')
    ax1.set_xlabel('Recall', fontsize='medium')
    ax1.set_ylabel('Precision', fontsize='medium')
    plt.savefig(os.path.join(output_dir, 'prcurve.png'))


def make_roc_chart(tp_rate, fp_rate, output_dir):
    fig1, ax1 = plt.subplots()
    ax1.plot(fp_rate, tp_rate)
    ax1.axis([0, 1, 0, 1])
    ax1.set_title('ROC Curve')
    plt.savefig(os.path.join(output_dir, 'roc_curve.png'))



def make_pie_charts(stats_map, output_dir):
    chart_num = 0
    for stat_key in stats_map:
        stats = stats_map[stat_key]
        fp_types = stats.keys()
        fig1, ax1 = plt.subplots()
        ax1.set_title(f'False positive distribution: {stat_key}')
        ax1.pie(stats.values(), labels=fp_types, autopct='%1.1f%%')
        ax1.axis('equal')
        plt.savefig(os.path.join(output_dir, f'pie{chart_num}.png'))
        chart_num += 1

@click.command()
@click.argument('xml_dir')
@click.argument('annotations_dir')
@click.argument('img_dir')
@click.argument('output_dir')
def convert(xml_dir, annotations_dir, img_dir, output_dir):
    fp_list = run_evaluate(xml_dir, annotations_dir, output_dir, img_dir=img_dir)
    smap = calculate_statistics_map(fp_list)
    make_pie_charts(smap, output_dir)


if __name__ == '__main__':
    convert()
    

