import pandas as pd
import numpy as np
from converters.xml2list import xml2list
from os import listdir
from os.path import splitext, join

def ingest_file(path):
    """
    Ingest an XML file to a dataframe
    :param path: path to XML file
    :return: dataframe of [id, label, x0,y0,x1,y1]
    """
    lst = xml2list(path)
    labels = [item[0] for item in lst]
    coords = [item[1] for item in lst]
    x0 = [coord[0] for coord in coords]
    y0 = [coord[1] for coord in coords]
    x1 = [coord[2] for coord in coords]
    y1 = [coord[3] for coord in coords]
    return pd.DataFrame({
            "label": labels,
            "x0": x0,
            "x1": x1,
            "y0": y0,
            "y1": y1
        })

def get_ious(pred_df, box):
    X = 0
    Y = 1
    X2 = 2
    Y2 = 3
    xml_box = pred_df[["x0", "y0", "x1", "y1"]].values
    N, _ = xml_box.shape
    i_boxes = np.zeros((N,4))
    i_boxes[:, X] = np.maximum(xml_box[:, X].reshape(-1), box[X].reshape(-1))
    i_boxes[:, Y] = np.maximum(xml_box[:, Y].reshape(-1), box[ Y].reshape(-1))
    i_boxes[:, X2] = np.minimum(xml_box[:, X2].reshape(-1), box[X2].reshape(-1))
    i_boxes[:, Y2] = np.minimum(xml_box[:, Y2].reshape(-1), box[Y2].reshape(-1))
    i_area = (i_boxes[:, X2] - i_boxes[:, X]) * (i_boxes[:, Y2] - i_boxes[:, Y])
    i_area[i_boxes[:, X2] < i_boxes[:, X]] = 0
    i_area[i_boxes[:, Y2] < i_boxes[:, Y]] = 0
    xml_area = (xml_box[:, X2] - xml_box[:, X]) * (xml_box[:, Y2] - xml_box[:, Y])
    box_area = (box[X2] - box[X]) * (box[Y2] - box[Y])
    iou = i_area/(xml_area+ box_area - i_area)
    iou_df = pd.DataFrame({
        "iou": iou
        })
    return iou_df

   

def match(pred_df, gt_df, thres=0.5):
    for idx, row in enumerate(gt_df.itertuples()):
        box = np.array([row.x0, row.y0, row.x1, row.y1])
        pred_df[f"iou_{idx}"] = get_ious(pred_df, box)
    overlaps = pred_df.filter(regex=("iou_*")).values
    matches = np.argmax(overlaps, axis=1)
    max_overlaps = np.array([pred_df.at[idx, f"iou_{best}"] for idx, best in enumerate(matches)])
    match_labels = [gt_df.at[match, "label"] for match in matches]
    mask =  max_overlaps < thres
    matches[mask] = -1.0
    pred_df["gt_id"] = matches
    pred_df["gt_label"] = match_labels
    pred_df["max_overlap"] = max_overlaps
    combined_df = pred_df.rename(index=str, columns={"label": "pred_label"})
    unmatched_idxs = list(filter(lambda x: x not in matches, range(gt_df.shape[0])))
    unmatched = gt_df.loc[unmatched_idxs]
    return combined_df, unmatched

def get_tp(combined_df, cls):
    """
    true positives have the correct label,
    only one tp per ground truth label
    """
    tp_candidates = combined_df[combined_df["pred_label"] == combined_df["gt_label"]]
    tp_candidates = tp_candidates[tp_candidates["pred_label"] == cls]
    groups = tp_candidates.groupby("gt_id")
    return float(len(groups))

def get_fp(combined_df, cls):
    fp_candidates = combined_df[combined_df["pred_label"] == cls] 
    fp_type_1 = fp_candidates[fp_candidates["pred_label"] != fp_candidates["gt_label"]].shape[0]
    fp_type_2 = 0.0    
    tp_candidates = combined_df[combined_df["pred_label"] == combined_df["gt_label"]]
    tp_candidates = tp_candidates[tp_candidates["pred_label"] == cls]
    groups = tp_candidates.groupby("gt_id")

    matches = len(groups)
    tot = groups.size()
    fp_type_2 += tot.sum() - matches
    return float(fp_type_1 + fp_type_2)


def get_fn(combined_df, unmatched, cls):
    fn_candidates = combined_df[combined_df["gt_label"] == cls]
    fn_type_1 = fn_candidates[fn_candidates["pred_label"] != fn_candidates["gt_label"] ].shape[0]
    fn_type_2 = unmatched[unmatched["label"] == cls].shape[0]
    return float(fn_type_1 + fn_type_2)


def get_precision(combined_df, cls):
    tp = get_tp(combined_df, cls)
    fp = get_fp(combined_df, cls)
    if tp == 0 and fp == 0:
        return np.nan
    return tp/(tp+fp)

def get_recall(combined_df, unmatched, cls):
    tp = get_tp(combined_df, cls)
    fn = get_fn(combined_df, unmatched, cls)
    if tp == 0 and fn == 0:
        return np.nan
    return tp / (tp +fn)

def get_gt_instances(gt_df, cls):
    return gt_df[gt_df["label"] == cls].shape[0]

def evaluate_single(pred_path, gt_path, classes=None, thres=0.5):
    pred_df = ingest_file(pred_path)
    gt_df = ingest_file(gt_path)
    combined , unmatched = match(pred_df, gt_df,thres=0.5)
    prec_cls = {}
    rec_cls = {}
    for cls in classes:
        prec = get_precision(combined, cls)
        rec = get_recall(combined, unmatched, cls)
        prec_cls[cls] = prec
        rec_cls[cls] = rec
    prec_df = pd.Series(prec_cls,name="precisions")
    prec_df["total"] = prec_df.mean(skipna=True)
    rec_df = pd.Series(rec_cls, name="recalls")
    rec_df["total"] = rec_df.mean(skipna=True)
    return prec_df, rec_df



def aggregate(result_dfs):
    result = pd.concat(result_dfs, axis=1).mean(axis=1,skipna=True)
    return result

def evaluate_dir(pred_dir, gt_dir, classes=None):
    files = listdir(pred_dir)
    identifiers = [splitext(fp)[0] for fp in files]
    results_prec = []
    results_rec = []
    for identifier in identifiers:
        pred_path = join(pred_dir, f"{identifier}.xml")
        gt_path = join(gt_dir, f"{identifier}.xml")
        precs, recs = evaluate_single(pred_path, gt_path, classes)
        results_prec.append(precs)
        results_rec.append(recs)
    final_df_prec = aggregate(results_prec)
    final_df_rec = aggregate(results_rec)
    df = pd.concat((final_df_prec, final_df_rec),axis=1)
    df.columns = ["precision", "recall"]
    print(df)
    return df
    

