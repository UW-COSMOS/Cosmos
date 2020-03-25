"""
Train postprocessing
"""

from argparse import ArgumentParser
import numpy as np
from ingest.process.detection.src.infer import get_model
from ingest.process.postprocess.xgboost_model.model import PostProcessTrainer
from ingest.process.postprocess.xgboost_model.featurizer import get_feat_vec
from xgboost import XGBClassifier
import yaml
from ingest.process.detection.src.converters.xml2list import xml2list
from ingest.process.ocr.ocr import run as ocr
from joblib import Parallel, delayed
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import glob
from ingest.process.detection.src.infer import run_inference
from ingest.process.detection.src.torch_model.train.data_layer.sql_types import Base
import os
from PIL import Image, ImageFile
from tqdm import tqdm
ImageFile.LOAD_TRUNCATED_IMAGES = True


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

def load_data_gt(predict_list, classes, gt_map):
    features = []
    targets = []
    for predict in predict_list:
        bb = predict[0]
        bb = tuple(bb)
        if bb in gt_map:
            if gt_map[bb] is None:
                continue
            targets.append(classes.index(gt_map[bb]))
            features.append(get_feat_vec(predict, predict_list, classes))
    print(f'Features: {features}')
    print(f'Targets: {targets}')

    return features, targets

def match_lists_train(bb_list, target_list):
    list_map = {}
    for bb in bb_list:
        # make a tuple because i need to hash predicitons
        bb = tuple(bb)
        # Calculate which output annotation has the highest IOU
        ious = [calculate_iou(bb, target[1]) for target in target_list]
        if len(ious) == 0:
            list_map[bb] = None
        else:
            max_iou = max(ious)
            if max_iou < 0.1:
                list_map[bb] = None
                continue
            for ind, iou in enumerate(ious):
                if iou == max_iou:
                    list_map[bb] = target_list[ind][0]
                    break
    return list_map

def featurize_obj(obj, xml_dir, classes):
    id = obj['id']
    img = obj['img']
    detected_objs = obj['detected_objs']
    tess_df, detect_objects = ocr(img, detected_objs)
    xpath = f'{id}.xml'
    gt_list = xml2list(os.path.join(xml_dir, xpath))
    gt_map = match_lists_train(obj['proposals'], gt_list)
    features, targets = load_data_gt(detect_objects, classes, gt_map)
    return features, targets

def featurize_images(image_dir, proposals_dir, xml_dir, model, model_config, device_str, classes, num_processes):
    results = []
    for f in tqdm(glob.glob(os.path.join(image_dir, '*'))):
        engine = create_engine('sqlite:///:memory:', echo=False)  
        Session = sessionmaker()
        Session.configure(bind=engine)
        Base.metadata.create_all(engine)
        session = Session()
        id = os.path.basename(f)[:-4]
        ppath = f'{id}.csv'
        with open(os.path.join(proposals_dir, ppath), 'r') as rf:
            proposals = []
            for l in rf:
                l = l[:-1]
                coords = l.split(',')
                coords = [int(c) for c in coords]
                proposals.append(coords)
        img = Image.open(f).convert('RGB')
        obj = {'img': img, 'proposals': proposals, 'id': id}
        detected_objs, _ = run_inference(model, [obj], model_config, device_str, session)
        detected_objs = detected_objs[id]
        results.append({'id': id, 'img': img, 'detected_objs': detected_objs, 'proposals': proposals})
        session.close()

    featurized = Parallel(n_jobs=num_processes)(delayed(featurize_obj)(r, xml_dir, classes) for r in results)
    final_features = []
    final_targets = []
    for features, targets in featurized:
        final_features.extend(features)
        final_targets.extend(targets)
    return np.array(final_features), np.array(final_targets)
    

if __name__ == '__main__':
    parser = ArgumentParser(description="Preprocess data for postprocessing")
    parser.add_argument("--logdir")
    parser.add_argument("--modelcfg")
    parser.add_argument("--weights")
    parser.add_argument("--device")
    parser.add_argument("--train_imgdir")
    parser.add_argument("--train_proposalsdir")
    parser.add_argument("--train_xmldir")
    parser.add_argument("--val_imgdir")
    parser.add_argument("--val_proposalsdir")
    parser.add_argument("--val_xmldir")
    parser.add_argument("--num_processes")
    parser.add_argument("--classcfg")
    args = parser.parse_args()
    with open(args.classcfg) as stream:
        classes  = yaml.load(stream)["classes"]
    model = get_model(args.modelcfg, args.weights, args.device)
    nump = int(args.num_processes)
    print("Featuring Training Dataset")
    train_x, train_y = featurize_images(args.train_imgdir, args.train_proposalsdir, args.train_xmldir, model, args.modelcfg, args.device, classes, nump)
    print("Featuring Validation Dataset")
    val_x, val_y = featurize_images(args.val_imgdir, args.val_proposalsdir, args.val_xmldir, model, args.modelcfg, args.device, classes, nump)
    print(val_x)
    print("Dataset featurized")
    model = XGBClassifier()
    print("Training model")
    trainer = PostProcessTrainer(model, train_x, train_y, val_x, val_y, classes, log_dir=args.logdir, model_path='/data/weights/pp_model_weights1.pth')

    trainer.train()

    print("Training completed")




    
    
