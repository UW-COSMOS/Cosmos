import sqlalchemy
import numpy as np
from ingest.process.detection.src.evaluate.evaluate import calculate_iou
from collections import namedtuple
import torch
from PIL import Image
import os
from xml.etree import ElementTree as ET
from ingest.process.detection.src.converters.xml2list import xml2list
from numpy import genfromtxt
from torchvision.transforms import ToTensor
from ingest.process.detection.src.torch_model.train.data_layer.transforms import NormalizeWrapper
from uuid import uuid4
from tqdm import tqdm
import pickle
from ingest.process.detection.src.torch_model.utils.matcher import match
from ingest.process.detection.src.torch_model.utils.bbox import BBoxes
from ingest.process.detection.src.torch_model.train.data_layer.sql_types import Example as Ex
from ingest.process.detection.src.torch_model.train.data_layer.sql_types import Neighbor, Base
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import yaml
import io
import logging

logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
Example = namedtuple('Example', ["ex_window", "ex_proposal", "gt_cls", "gt_box"])

normalizer = NormalizeWrapper()
tens = ToTensor()
#with open("config/classes.yaml") as stream:
#    classes = yaml.load(stream)["classes"]
#    print(f"classes are {classes}")

def mapper(obj, preprocessor=None):
    """
    map a single object to the list structure
    :param obj: an Etree node
    :return: (type, (x1, y1, x2, y2))
    """
    bnd = obj.find("bndbox")
    coords = ["xmin", "ymin", "xmax", "ymax"]
    pts = [int(float(bnd.find(coord).text)) for coord in coords]
    x1, y1, x2, y2 = pts
    w = x2 - x1
    h = y2 - y1
    # get centers
    x = x1 + w/2
    y = y1 + h/2
    cls = obj.find("name").text
    if preprocessor is not None:
        cls = preprocessor[cls]
    return cls, (x, y, h,w)


def xml2list(fp):
    """
    convert VOC XML to a list
    :param fp: file path to VOC XML file
    :return: [(type,(x1, y1, x2, y2))]
    """
    tree = ET.parse(fp)
    root = tree.getroot()
    objects = root.findall("object")
    lst = [mapper(obj) for obj in objects]
    lst.sort(key=lambda x: x[1])
    return lst

def load_gt(xml_dir, identifier):
    """
    Load an XML ground truth document
    :param xml_dir: base path to xml
    :param identifier: xml document identifier
    :return: [K x 4] Tensor, [cls_names]
    """
    path = os.path.join(xml_dir, f"{identifier}.xml")
    as_lst = xml2list(path)
    if len(as_lst) == 0:
        cls_list = [0]
        tensor_list = [[0,0,0,0]]
    else:
        cls_list, tensor_list = zip(*as_lst)
    # convert to tensors
    gt_boxes = BBoxes(torch.tensor(tensor_list),"xyhw")
    return gt_boxes, cls_list


def load_image(base_path, identifier, img_type):
    """
    load an image into memory
    :param base_path: base path to image
    :param identifier:
    :param img_type:
    :return: [3 x SIZE x SIZE] tensor
    """
    path = os.path.join(base_path, f"{identifier}{img_type}")
    pil = Image.open(path)
    return pil

def load_proposal(base_path, identifier):
    """
    Load a set of proposals into memory
    :param base_path: path to proposals dir
    :param identifier: name of proposals file to open
    :return: BBoxes object denoted proposal
    """
    path = os.path.join(base_path, f"{identifier}.csv")
    np_arr = genfromtxt(path, delimiter=",")
    bbox_absolute = torch.from_numpy(np_arr).reshape(-1,4)
    return BBoxes(bbox_absolute, "xyxy")

def load_proposal_obj(obj):
    """
    Load a set of proposals into memory
    :param obj: Object to load proposals off of
    :return: BBoxes object denoted proposal
    """
    if obj['proposals'] is None:
        return None
    np_arr = np.array(obj['proposals'])
    bbox_absolute = torch.from_numpy(np_arr).reshape(-1,4)
    return BBoxes(bbox_absolute, "xyxy")

def unpack_page(page, warped_size):
    """
    Unpack the objects in a page object
    :param page: Page object to unpack
    :param warped_size: Warped size of image
    :return: unpacked ExampleData object
    """
    img, gt, proposals, identifier = page
    labels = None
    gt_box_lst = None
    matches = None
    if gt is not None:
        gt_boxes, gt_cls = gt
        matches = match(proposals,gt_boxes)
        labels = [gt_cls[match] for match in matches]
        # new_labels = []
        # for idx, label in enumerate(labels):
            # if matches[idx] != -1:
                # new_labels.append(label)
        # labels = new_labels
        gt_box_lst = gt_boxes.tolist()
        #filter 0 overlap examples
        mask = matches != -1
        idxs = mask.nonzero()
        idxs = idxs.squeeze()
        proposals.change_format("xyxy")
        # proposals = proposals[idxs, :].reshape(-1,4)
        #matches = list(filter(lambda x: x != -1, matches))
    windows = []
    proposals_lst = proposals.tolist()
    for idx, proposal in enumerate(proposals_lst):
        proposal = [int(c) for c in proposal]
        img_sub = img.crop(proposal)
        img_sub = img_sub.resize((warped_size, warped_size))
        img_data = tens(img_sub)
        img_data = normalizer(img_data)
        windows.append(img_data)
    # switch to list of tensors
    proposals_lst = [torch.tensor(prop) for prop in proposals_lst]
    match_box_lst = None
    if gt is not None:
        match_box_lst = []
        for idx in range(len(proposals_lst)):
            match_box_lst.append(torch.tensor(gt_box_lst[matches[idx]] if matches[idx] != -1 else 0))
    else:
        labels = [None for n in proposals_lst]
        match_box_lst = [None for n in proposals_lst]
    collected = list(zip(windows,proposals_lst,labels,match_box_lst))
    assert len(labels) == len(proposals_lst)
    ret = [Example(*pt) for pt in collected]
    ExampleData = namedtuple('ExampleData', 'examples proposals_len gt_box_len')
    return ExampleData(examples=ret, proposals_len=len(proposals_lst), gt_box_len=(len(gt_box_lst) if gt_box_lst is not None else 0))


def db_ingest(img_dir, proposal_dir, xml_dir, warped_size, partition, session):
    """
    ingest the db
    :param img_dir: Image directory
    :param proposal_dir: Proposal directory
    :param xml_dir: Path to annotations for train mode, None otherwise
    :param warped_size: Size of warped image
    :param partition: For train mode this corresponds to train/val/test
    :param session: DB Session to work on
    :return: IngestObjs containing statistics about the DB
    """
    class_stats = {}
    uuids = []
    nproposals = 0
    ngt_boxes = 0
    examples = []
    for img_name in tqdm(os.listdir(img_dir)):
        name, ext = os.path.splitext(img_name)
        image = load_image(img_dir, name, ext)
        gt = None
        proposals = None
        if xml_dir is not None:
            gt = load_gt(xml_dir, name)
        if proposal_dir is not None:
            proposals = load_proposal(proposal_dir, name)
            if proposals.shape[0] == 0:
                print("no proposals for ", name)
                continue
        ret = [image, gt, proposals, name]
        pts, proposals_len, gt_box_len = unpack_page(ret, warped_size)
        nproposals += proposals_len
        ngt_boxes += gt_box_len
        for pt in pts:
            uuid = str(uuid4())
            label = pt.gt_cls
            if label == 0 or label == "0":
                print("found 0")
                continue
            if label is not None and xml_dir is not None:
                uuids.append(uuid)
            elif xml_dir is None:
                uuids.append(uuid)
            if label in class_stats:
                class_stats[label] += 1
            else:
                class_stats[label] = 1
            ex = Ex(page_id=name, object_id=uuid, window=pt.ex_window, bbox=pt.ex_proposal, gt_box=pt.gt_box, label=pt.gt_cls, partition=partition)
            examples.append(ex)
    session.add_all(examples)
    session.commit()
    session.close()
    IngestObjs = namedtuple('IngestObjs', 'uuids class_stats nproposals ngt_boxes')
    return IngestObjs(uuids=uuids, class_stats=class_stats, nproposals=nproposals, ngt_boxes=ngt_boxes)


def db_ingest_objs(objs, warped_size, partition, session):
    """
    ingest the db
    :param objs: Objects to ingest
    :param warped_size: Size of warped image
    :param partition: For train mode this corresponds to train/val/test
    :param session: DB Session to work on
    :return: IngestObjs containing statistics about the DB
    """
    class_stats = {}
    uuids = []
    nproposals = 0
    ngt_boxes = 0
    examples = []
    for obj in objs:
        if 'img' in obj:
            image = obj['img']
        else:
            bstring = obj['padded_bytes']
            image = Image.open(io.BytesIO(bstring))
        proposals = load_proposal_obj(obj)
        if proposals is None or proposals.shape[0] == 0:
            continue
        ret = [image, None, proposals, None]
        pts, proposals_len, gt_box_len = unpack_page(ret, warped_size)
        nproposals += proposals_len
        ngt_boxes += gt_box_len
        for pt in pts:
            uuid = str(uuid4())
            label = pt.gt_cls
            if label == 0 or label == "0":
                print("found 0")
                continue
            uuids.append(uuid)
            if label in class_stats:
                class_stats[label] += 1
            else:
                class_stats[label] = 1
            ex = Ex(page_id=str(obj['id']), object_id=uuid, window=pt.ex_window, bbox=pt.ex_proposal, gt_box=pt.gt_box, label=pt.gt_cls, partition=partition)
            examples.append(ex)
    session.add_all(examples)
    session.commit()
    IngestObjs = namedtuple('IngestObjs', 'uuids class_stats nproposals ngt_boxes')
    return IngestObjs(uuids=uuids, class_stats=class_stats, nproposals=nproposals, ngt_boxes=ngt_boxes)


def get_example_for_uuid(uuid, session):
    """
    Helper function to fetch an example for a uuid
    :param uuid: input uuid
    :param session: DB Session
    :return: DB Example object
    """
    ex = session.query(Ex).filter_by(object_id=uuid).one()
    return ex


def compute_neighborhoods(partition, expansion_delta, session, orig_size=1920):
    """
    Compute the neighborhoods for a target image and input to DB
    :param session: DB Session
    :param partition: train/val/test partition
    :param expansion_delta: Neighborhood expansion parameter
    :param orig_size: original size of the image
    """
    print('Computing neighborhoods')
    avg_nbhd_size = 0.0
    nbhds = 0.0
    partition_filter = session.query(Ex).filter(Ex.partition == partition)
    for ex in tqdm(partition_filter):
        orig_bbox = ex.bbox
        nbhd_bbox = [max(0, orig_bbox[0]-expansion_delta), max(0, orig_bbox[1]-expansion_delta), min(orig_size, orig_bbox[2]+expansion_delta), min(orig_size, orig_bbox[3]+expansion_delta)]
        # Get all the examples on the same page
        nbhd = []
        for page_ex in session.query(Ex).filter(Ex.partition == ex.partition).filter(Ex.page_id == ex.page_id).filter(Ex.object_id != ex.object_id):
            target_bbox = page_ex.bbox
            iou = calculate_iou(nbhd_bbox, target_bbox)
            if iou > 0:
                nbhr = Neighbor(center_object_id=ex.object_id, neighbor_object_id=page_ex.object_id)
                nbhd.append(nbhr)
        nbhds += 1.0
        avg_nbhd_size += len(nbhd)
        session.add_all(nbhd)
    session.commit()
    print("=== Done Computing Neighborhoods ===")
    if nbhds != 0.0:
        print(f"Average of {avg_nbhd_size/nbhds} neighbors")

def get_neighbors_for_uuid(uuid, session):
    """
    Helper function to get neighbors for a uuid
    :param uuid: Input uuid
    :param session: Input DB Session
    :return: neighbors
    """
    ex = get_example_for_uuid(uuid, session)
    return ex.neighbors



class ImageDB:
    """
    SQL alchemy session factory
    """

    @staticmethod
    def initialize_and_ingest(objs, warped_size, partition, expansion_delta, session):
        """
        Initialize and ingest the db from the inputs
        :param objs: Either already ingested objects or a tuple of directories
        :param warped_size: Size to warp to
        :param partition: Partition if training
        :param expansion_delta: Neighborhood expansion parameter
        :return: database statistics (IngestObjs object)
        """
        # For training, we check the type so we can read from files
        ingest_objs = None
        if type(objs) == tuple:
            img_dir, proposal_dir, xml_dir = objs
            ingest_objs = db_ingest(img_dir, proposal_dir, xml_dir, warped_size, partition, session)
        # Alternatively, we can pass in a list of objects and call that loading function
        else:
            ingest_objs = db_ingest_objs(objs, warped_size, partition, session)
        compute_neighborhoods(partition, expansion_delta, session)
        return ingest_objs

    @staticmethod 
    def cleanup(objs, session):
        uuids = objs.uuids
        for uuid in uuids:
            session.query(Ex).filter_by(object_id=uuid).delete()
        session.commit()


