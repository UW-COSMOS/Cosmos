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
import redis
import torch
from torch.nn.utils.rnn import pad_sequence
from xml.etree import ElementTree as ET
from .transforms import NormalizeWrapper
import pickle
from torch_model.utils.matcher import match
from collections import namedtuple
from dataclasses import dataclass
from uuid import uuid4
from tqdm import tqdm
from torch_model.utils.bbox import BBoxes
from ingestion.ingest_images import db_ingest, get_example_for_uuid, compute_neighborhoods, ImageDB
from sqlalchemy.orm import sessionmaker
from sqlalchemy import create_engine

normalizer = NormalizeWrapper()

tens = ToTensor()
#Example = namedtuple('Example', ['center_bb', 'label', 'center_window', 'neighbor_boxes', 'neighbor_windows'])
@dataclass
class Example:
  center_bb: torch.Tensor
  label: torch.Tensor
  center_window: torch.Tensor
  neighbor_boxes: torch.Tensor
  neighbor_windows: torch.Tensor

Batch = namedtuple('Batch', ['center_bbs', 'labels', 'center_windows', 'neighbor_boxes', 'neighbor_windows'])




class XMLLoader(Dataset):
    """
    Loads examples and ground truth from given directories
    it is expected that the directories have no files
    other than annotations
    """

    def __init__(self, session, ingest_objs,classes):
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
        #print(len(neighbors), " neighbors")
        neighbor_boxes = [n.bbox for n in neighbors]
        neighbor_windows = [n.window for n in neighbors]
        if len(neighbors) == 0:
           neighbor_boxes = [torch.zeros(4), torch.zeros(4)]
           neighbor_windows = [torch.zeros(ex.window.shape), torch.zeros(ex.window.shape)]
        label = torch.Tensor([self.classes.index(ex.label)])
        return Example(ex.bbox, label, ex.window, neighbor_boxes, neighbor_windows)

    @staticmethod
    def collate(batch):
        center_bbs = torch.stack([ex.center_bb for ex in batch])
        labels = torch.stack([ex.label for ex in batch])
        center_windows = torch.stack([ex.center_window for ex in batch])
        # padding will put number of neighbors before the batch size
        neighbor_boxes = pad_sequence([torch.stack(ex.neighbor_boxes) for ex in batch]).permute(1,0,2)
        neighbor_windows = [torch.stack(ex.neighbor_windows) for ex in batch]
        neighbor_windows = pad_sequence(neighbor_windows).permute(1,0,2,3,4)
        return Batch(center_bbs=center_bbs, labels=labels, center_windows=center_windows, neighbor_boxes=neighbor_boxes, neighbor_windows=neighbor_windows)

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



