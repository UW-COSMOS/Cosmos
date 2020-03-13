"""
An XML and image repo loader
meant to work with the GTDataset class
Author: Josh McGrath
"""
from torch.utils.data import Dataset
import os
from os.path import splitext
from PIL import Image
from torchvision.transforms import ToTensor
from numpy import genfromtxt
import torch
from torch.nn.utils.rnn import pad_sequence
from xml.etree import ElementTree as ET
from .transforms import NormalizeWrapper
import pickle
from collections import namedtuple
from uuid import uuid4
from tqdm import tqdm
from ingest.process.detection.src.torch_model.utils.bbox import BBoxes
from ingest.process.detection.src.torch_model.utils.matcher import match
from ingest.process.detection.src.utils.ingest_images import db_ingest, get_example_for_uuid, compute_neighborhoods, ImageDB
from dataclasses import dataclass

normalizer = NormalizeWrapper()

tens = ToTensor()
@dataclass
class Example:
  center_bb: torch.Tensor
  label: torch.Tensor
  center_window: torch.Tensor
  neighbor_boxes: torch.Tensor
  neighbor_windows: torch.Tensor
  neighbor_radii: torch.Tensor
  neighbor_angles: torch.Tensor
  colorfulness: torch.Tensor

Batch = namedtuple('Batch', ['center_bbs', 'labels', 'center_windows', 'neighbor_boxes', 'neighbor_windows', 'neighbor_radii', 'neighbor_angles', 'colorfulness'])

def containsNone(lst):
    flag = False
    for o in lst:
        if o is None:
            flag = True
    return flag

def get_colorfulness(window):
    diffs = window.max(dim=0)[0] - window.min(dim=0)[0]
    return torch.mean(diffs.topk(25)[0])

def get_radii(center_bbox, neighbor_bboxes):
    center_bbox = center_bbox.reshape(1,4)
    assert center_bbox.shape[0] == 1
    assert center_bbox.shape[1] == 4
    assert neighbor_bboxes.shape[1] == 4
    diffs = center_bbox - neighbor_bboxes
    return torch.norm(diffs, p=2, dim=1)

def get_angles(center_bbox, neighbor_boxes):
    radii = get_radii(center_bbox, neighbor_boxes)
    # get center coords of center box
    center_bbox = center_bbox.reshape(1,4)
    center_center = torch.stack([center_bbox[:,0] -center_bbox[:,2], center_bbox[:,1] - center_bbox[:,3]])
    neighbor_center = torch.stack([neighbor_boxes[:,0] -neighbor_boxes[:,2], neighbor_boxes[:,1] - neighbor_boxes[:,3]])
    # clamp required to not produce nan at asin
    delta_y = torch.abs(center_center[1] - neighbor_center[1])
    ratios = delta_y/radii
    out = torch.asin(ratios.clamp(-1 + 1e-4, 1- 1e-4))
    mask = out != out
    out[mask] = 0.4
    return out

class XMLLoader(Dataset):
    """
    Loads examples and ground truth from given directories
    it is expected that the directories have no files
    other than annotations
    """

    def __init__(self, ingest_objs, classes, session):
        """
        Initialize a XML loader object
        :param xml_dir: directory to get XML from
        :param img_dir: directory to load PNGs from
        :param img_type: the image format to load in
        """
        self.session = session
        self.uuids = ingest_objs.uuids
        self.ngt_boxes = ingest_objs.ngt_boxes
        self.nproposals = ingest_objs.nproposals
        self.class_stats = ingest_objs.class_stats
        self.classes = classes
        print("printing class stats")
        self.print_stats()
        print(f"# of gt boxes:{self.ngt_boxes}")
        print(f"# of proposals:{self.nproposals}")


    def __len__(self):
        return len(self.uuids)

    def __getitem__(self, item):
        uuid = self.uuids[item]
        ex = get_example_for_uuid(uuid, self.session)
        neighbors = ex.neighbors(True, self.uuids, self.session)
        colorfulness = get_colorfulness(ex.window)
        #print(len(neighbors), " neighbors")
        if len(neighbors) == 0:
           neighbor_boxes = [torch.zeros(4), torch.zeros(4)]
           neighbor_windows = [torch.zeros(ex.window.shape), torch.zeros(ex.window.shape)]
           neighbor_radii = torch.tensor([-1*torch.ones(1)] *2)
           neighbor_angles = neighbor_radii
        else:
           neighbor_boxes = [n.bbox for n in neighbors]
           neighbor_windows = [n.window for n in neighbors]
           neighbor_radii = get_radii(ex.bbox, torch.stack(neighbor_boxes))
           neighbor_angles = get_angles(ex.bbox, torch.stack(neighbor_boxes))
        label = torch.Tensor([self.classes.index(ex.label)]) if ex.label is not None else None
        return Example(ex.bbox, label, ex.window, neighbor_boxes, neighbor_windows, neighbor_radii, neighbor_angles,colorfulness)

    @staticmethod
    def collate(batch):
        center_bbs = torch.stack([ex.center_bb for ex in batch])
        ex_labels = [ex.label for ex in batch]
        labels = None
        if containsNone(ex_labels):
            labels = None
        else:
            labels = torch.stack([ex.label for ex in batch])
        center_windows = torch.stack([ex.center_window for ex in batch])
        colorfulness = torch.stack([ex.colorfulness for ex in batch])
        # padding will put number of neighbors before the batch size
        neighbor_boxes = pad_sequence([torch.stack(ex.neighbor_boxes) for ex in batch]).permute(1,0,2)
        neighbor_windows = [torch.stack(ex.neighbor_windows) for ex in batch]
        neighbor_windows = pad_sequence(neighbor_windows).permute(1,0,2,3,4)
        neighbor_radii =  pad_sequence([ex.neighbor_radii for ex in batch], padding_value=-1).permute(1,0)
        neighbor_angles = pad_sequence([ex.neighbor_angles for ex in batch], padding_value=-1).permute(1,0)
        return Batch(center_bbs=center_bbs, labels=labels, center_windows=center_windows, neighbor_boxes=neighbor_boxes, neighbor_windows=neighbor_windows,neighbor_radii=neighbor_radii, neighbor_angles=neighbor_angles, colorfulness=colorfulness)

    def get_weight_vec(self, classes):
        weight_per_class = {}                                    
        N = len(self.uuids)
        for name in classes:
            if name not in self.class_stats:
                weight_per_class[name] = 0
            else:
                weight_per_class[name] = N/float(self.class_stats[name])                                 
        weight = [0] * N                                              
        for idx, uuid in tqdm(enumerate(self.uuids)):
            lst = get_example_for_uuid(uuid, self.session)
            weight[idx] = weight_per_class[lst.label]
        return weight


    def print_stats(self):
        tot = len(self.uuids)
        print(f"There are {tot} objects")
        for key in self.class_stats:
            sub = self.class_stats[key]
            percentage = float(sub)/tot
            print(f"{key}: {sub} ({percentage})")



