"""
Adapters to run Pascal VOC mAP and
COCO mAP on this platform
Author: Josh McGrath
"""
from .ingestion import ingest_file
import os
from chainercv.evaluations import eval_detection_voc, eval_detection_coco
import numpy as np
from tqdm import tqdm
import yaml
import pandas as pd
import click

def get_identifiers(dir):
    files = os.listdir(dir)
    return [os.path.splitext(f)[0] for f in files]

def ingest(pred_dir, gt_dir, identifier):
    pred_path = os.path.join(pred_dir, f"{identifier}.xml")
    gt_path = os.path.join(gt_dir, f"{identifier}.xml")
    pred_df = ingest_file(pred_path)
    gt_df = ingest_file(gt_path)
    return pred_df, gt_df

def map_to_integers(labels, classes):
    labels = labels
    result = labels.apply(lambda row: classes.index(row))
    return result

def format_result(eval_result, classes):
    aps = list(eval_result["ap"])
    map = eval_result["map"]
    aps.append(map)
    classes.append("mAP")
    print(aps)
    print(classes)
    print(len(aps))
    print(len(classes))
    results_df = pd.DataFrame({
        "class": classes,
        "AP": aps
        })
    print(results_df)
    return results_df

def run_eval(pred_dir, gt_dir, eval_func, classes):
    identifiers = get_identifiers(pred_dir)
    pred_labels = []
    pred_bboxes = []
    gt_labels = []
    gt_bboxes = []
    scores = []
    for identifier in tqdm(identifiers):
        pred_df, gt_df = ingest(pred_dir, gt_dir, identifier)
        pred_bbox = pred_df[["x0", "y0", "x1", "y1"]].values
        npred = pred_df.shape[0]
        pred_label = map_to_integers(pred_df["label"], classes).values
        score = pred_df['score'].values
        gt_bbox  = gt_df[["x0", "y0", "x1", "y1"]].values
        gt_label = map_to_integers(gt_df["label"], classes).values
        pred_labels.append(pred_label)
        pred_bboxes.append(pred_bbox)
        gt_labels.append(gt_label)
        gt_bboxes.append(gt_bbox)
        scores.append(score)
    eval_result = eval_func(pred_bboxes, pred_labels, scores, gt_bboxes, gt_labels)
    return format_result(eval_result, classes) 


def run_voc(pred_dir, gt_dir, classes):
    return run_eval(pred_dir, gt_dir,eval_detection_voc, classes) 

def run_coco(pred_dir, gt_dir, classes):
    return run_eval(pred_dir, gt_dir, eval_detection_coco, classes)


@click.command()
@click.argument('pred_dir')
@click.argument('annotation_dir')
def run_evaluate_voc(pred_dir, annotation_dir):
    classes = [ 'Figure Caption',
                'Figure',
                'Table Caption',
                'Table',
                'Body Text',
                'Page Footer',
                'Page Header',
                'Equation',
                'Section Header',
                'Reference text',
                'Other'
              ]
    df = run_voc(pred_dir, output_dir, classes)


if __name__ == '__main__':
    run_evaluate_voc()

