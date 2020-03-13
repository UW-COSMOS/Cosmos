"""
Utilities for loading inference data into the model
"""
# TODO refactor so xml_loader and inference_loader import from a utilities directory
from ingest.process.detection.src.utils.ingest_images import load_image, load_proposal, get_example_for_uuid, ImageDB
from torch.utils.data import Dataset
import torch
import os
from os.path import splitext
from ingest.process.detection.src.torch_model.train.data_layer.xml_loader import XMLLoader
from torchvision.transforms import ToTensor
from ingest.process.detection.src.torch_model.train.data_layer.transforms import NormalizeWrapper
from collections import namedtuple

normalizer = NormalizeWrapper(mean=[0.485, 0.456, 0.406],std=[0.229, 0.224, 0.225])
tens = ToTensor()
Document = namedtuple("Document", ["windows", "proposals", "identifier"])

class InferenceLoader(XMLLoader):
    """
    Inference dataset object, based on XMLLoader
    """

    def __init__(self, ingest_objs, classes, session):
        """
        Init function
        :param session: DB Session
        :param ingest_objs: Database statistics object
        :param classes: List of classes
        """
        super().__init__(ingest_objs, classes, session)

    @staticmethod
    def collate(batch):
        """
        collation function to be used with this dataset class
        :param batch:
        :return:
        """
        if len(batch) > 1:
            raise ValueError(f"Inference classes are only meant to be used with a batch size of 1, got {len(batch)}")
        example = [batch[0][0]]
        collated = XMLLoader.collate(example)
        return collated, batch[0][1]

    def __getitem__(self, item):
        """
        Get an item
        :param item: UUID index
        :return: XMLLoader exmaple, as well as InferenceLoader example
        """
        example = super(InferenceLoader, self).__getitem__(item)
        uuid = self.uuids[item]
        ex_db = get_example_for_uuid(uuid, self.session)
        return example, ex_db

#
