import sqlalchemy
from collections import namedtuple
import torch
from torch_model.utils.bbox import BBoxes
from PIL import Image
import redis
import os
from xml.etree import ElementTree as ET
from converters.xml2list import xml2list
from numpy import genfromtxt
from torch_model.utils.matcher import match
from torchvision.transforms import ToTensor
from torch_model.train.data_layer.transforms import NormalizeWrapper
from uuid import uuid4
from tqdm import tqdm
import pickle
from torch_model.train.data_layer.sql_types import Example as Ex

Example = namedtuple('Example', ["ex_window", "ex_proposal", "gt_cls", "gt_box"])
normalizer = NormalizeWrapper()
tens = ToTensor()


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

    """
    path = os.path.join(base_path, f"{identifier}.csv")
    np_arr = genfromtxt(path, delimiter=",")
    bbox_absolute = torch.from_numpy(np_arr).reshape(-1,4)
    return BBoxes(bbox_absolute, "xyxy")

def unpack_page(page, warped_size):
    img, gt, proposals, identifier = page
    gt_boxes, gt_cls = gt
    matches = match(proposals,gt_boxes)
    #filter 0 overlap examples
    mask = matches != -1
    idxs = mask.nonzero()
    idxs = idxs.squeeze()
    proposals.change_format("xyxy")
    proposals = proposals[idxs, :].reshape(-1,4)
    matches = list(filter(lambda x: x != -1, matches))
    labels = [gt_cls[match] for match in matches]
    windows = []
    proposals_lst = proposals.tolist()
    gt_box_lst = gt_boxes.tolist()
    for idx, proposal in enumerate(proposals_lst):
        proposal = [int(c) for c in proposal]
        img_sub = img.crop(proposal)
        img_sub = img_sub.resize((warped_size, warped_size))
        img_data = tens(img_sub)
        img_data = normalizer(img_data)
        windows.append(img_data)
    # switch to list of tensors
    proposals_lst = [torch.tensor(prop) for prop in proposals_lst]
    match_box_lst = []
    for idx in range(len(proposals_lst)):
        match_box_lst.append(torch.tensor(gt_box_lst[matches[idx]]))
    collected = list(zip(windows,proposals_lst,labels,match_box_lst))
    ret = [Example(*pt) for pt in collected]
    ExampleData = namedtuple('ExampleData', 'examples proposals_len gt_box_len')
    return ExampleData(examples=ret, proposals_len=len(proposals_lst), gt_box_len=len(gt_box_lst))

def redis_get(uuid, pool):
    conn = redis.Redis(connection_pool=pool)
    bytes_rep = conn.get(uuid)
    lst = pickle.loads(bytes_rep)
    return lst


def redis_ingest(img_dir, proposal_dir, xml_dir, warped_size, pool):
    img_names = os.listdir(img_dir)
    conn = redis.Redis(connection_pool=pool)
    class_stats = {}
    uuids = []
    nproposals = 0
    ngt_boxes = 0
    for img_name in tqdm(img_names):
        name, ext = os.path.splitext(img_name)
        image = load_image(img_dir, name, ext)
        gt = None
        proposals = None
        if xml_dir is not None:
            gt = load_gt(xml_dir, name)
        if proposal_dir is not None:
            proposals = load_proposal(proposal_dir, name)
        ret = [image, gt, proposals, name]
        pts, proposals_len, gt_box_len = unpack_page(ret, warped_size)
        nproposals += proposals_len
        ngt_boxes += gt_box_len
        for pt in pts:
            uuid = str(uuid4())
            uuids.append(uuid)
            label = pt.gt_cls
            if label in class_stats:
                class_stats[label] += 1
            else:
                class_stats[label] = 1
            obj = pickle.dumps(pt)
            conn.set(uuid, obj)
    IngestObjs = namedtuple('IngestObjs', 'uuids class_stats nproposals ngt_boxes')
    return IngestObjs(uuids=uuids, class_stats=class_stats, nproposals=nproposals, ngt_boxes=ngt_boxes)


def db_ingest(img_dir, proposal_dir, xml_dir, warped_size, session):
    class_stats = {}
    uuids = []
    nproposals = 0
    ngt_boxes = 0
    examples = []
    for img_name in tqdm(img_names):
        name, ext = os.path.splitext(img_name)
        image = load_image(img_dir, name, ext)
        gt = None
        proposals = None
        if xml_dir is not None:
            gt = load_gt(xml_dir, name)
        if proposal_dir is not None:
            proposals = load_proposal(proposal_dir, name)
        ret = [image, gt, proposals, name]
        pts, proposals_len, gt_box_len = unpack_page(ret, warped_size)
        nproposals += proposals_len
        ngt_boxes += gt_box_len
        for pt in pts:
            uuid = str(uuid4())
            uuids.append(uuid)
            label = pt.gt_cls
            if label in class_stats:
                class_stats[label] += 1
            else:
                class_stats[label] = 1
            obj = pickle.dumps(pt)
            ex = Ex(page_id=name, object_id=uuid, window=pt['ex_windows'], bbox=pt['ex_proposal'], gt_box=pt['gt_box'], label=pt['gt_cls'])
            examples.append(ex)
    session.add_all(examples)
    IngestObjs = namedtuple('IngestObjs', 'uuids class_stats nproposals ngt_boxes')
    return IngestObjs(uuids=uuids, class_stats=class_stats, nproposals=nproposals, ngt_boxes=ngt_boxes)

def db_get(uuid, session):
    ex = session.query(Ex).filter_by(object_id=uuid).one()
    example = Example(ex_window=ex.window, ex_proposal=ex.bbox, gt_cls=ex.label, gt_box=ex.gt_box)
    return example



