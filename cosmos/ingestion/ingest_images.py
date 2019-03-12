import psycopg2
from collections import namedtuple
import torch
from torch_model.utils.bbox import BBoxes
from PIL import Image
import redis
import os


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
    path = os.path.join(base_path, f"{identifier}.{img_type}")
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

def unpack_page(self, page, warped_size):
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


def redis_ingest(img_dir, proposal_dir, xml_dir, warped_size, host):
    img_names = os.listdir(img_dir)
    pool = redis.ConnectionPool(host=host)
    conn = redis.Redis(pool=pool)
    class_stats = {}
    uuids = []
    nproposals = 0
    ngt_boxes = 0
    for img_name in img_names:
        name, ext = splitext(img_name)
        image = load_image(img_dir, name, ext)
        gt = None
        proposals = None
        if xml_dir is not None:
            gt = load_gt(xml_dir, identifier)
        if proposal_dir is not None:
            proposals = load_proposal(proposal_dir, identifier)
        ret = [img, gt, proposals, identifier]
        pts, proposals_len, gt_box_len = unpack_page(ret)
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


def pg_ingest(img_dir, proposals_dir):
    pass



