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
from xml.etree import ElementTree as ET
from .transforms import NormalizeWrapper
import pickle
from torch_model.utils.matcher import match
from collections import namedtuple
from uuid import uuid4
from tqdm import tqdm
from torch_model.utils.bbox import BBoxes
from ingestion.ingest_images import redis_ingest, redis_get

normalizer = NormalizeWrapper()

tens = ToTensor()
Example = namedtuple('Example', ["ex_window", "ex_proposal", "gt_cls", "gt_box"])



class XMLLoader(Dataset):
    """
    Loads examples and ground truth from given directories
    it is expected that the directories have no files
    other than annotations
    """


    def __init__(self, img_dir, xml_dir=None, proposal_dir=None, warped_size=300, img_type="jpg", host="redis", debug=True):
        """
        Initialize a XML loader object
        :param xml_dir: directory to get XML from
        :param img_dir: directory to load PNGs from
        :param img_type: the image format to load in
        """
        self.debug = debug
        self.xml_dir = xml_dir
        self.img_dir = img_dir
        self.proposal_dir = proposal_dir
        self.img_type = img_type
        self.warped_size = warped_size
        self.imgs = os.listdir(img_dir)
        self.identifiers = [splitext(img)[0] for img in self.imgs]
        self.uuids = []
        self.num_images = len(self.imgs)
        self.pool = redis.ConnectionPool(host=host)
        self.no_overlaps = []
        self.ngt_boxes = 0
        self.nproposals = 0
        print(f"Constructed a {self.num_images} image dataset, ingesting to redis server")
        self.class_stats = {}
        self._ingest()
        print("ingested to redis, printing class stats")
        self.print_stats()
        print(f"# of gt boxes:{self.ngt_boxes}")
        print(f"# of proposals:{self.nproposals}")
        with open("no_overlaps.txt","w") as fh:
            for identifier in self.no_overlaps:
                fh.write(f"{identifier}\n")
        

    def __len__(self):
        return len(self.uuids)

    def __getitem__(self, item):
        uuid = self.uuids[item]
        return redis_get(uuid, self.pool)


    def get_weight_vec(self, classes):
        weight_per_class = {}                                    
        N = len(self.uuids)
        for name in classes:                                                   
            weight_per_class[name] = N/float(self.class_stats[name])                                 
        weight = [0] * N                                              
        for idx, uuid in tqdm(enumerate(self.uuids)):                                          
            conn = redis.Redis(connection_pool=self.pool)
            bytes_rep = conn.get(uuid)
            lst = pickle.loads(bytes_rep)
            weight[idx] = weight_per_class[lst.gt_cls]
        return weight


    def _ingest(self):
        self.uuids, self.class_stats, self.nproposals, self.ngt_boxes = redis_ingest(self.img_dir, self.proposal_dir, self.xml_dir, self.warped_size, self.pool)


    def print_stats(self):
        tot = len(self.uuids)
        print(f"There are {tot} objects")
        for key in self.class_stats:
            sub = self.class_stats[key]
            percentage = float(sub)/tot
            print(f"{key}: {sub} ({percentage})")



