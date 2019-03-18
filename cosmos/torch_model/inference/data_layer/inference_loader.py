"""
Utilities for loading inference data into the model
"""
# TODO refactor so xml_loader and inference_loader import from a utilities directory
from ingestion.ingest_images import load_image, load_proposal, get_example_for_uuid
from torch.utils.data import Dataset
import torch
import os
from os.path import splitext
from torch_model.train.data_layer.xml_loader import XMLLoader
from torchvision.transforms import ToTensor
from torch_model.train.data_layer.transforms import NormalizeWrapper
from collections import namedtuple

normalizer = NormalizeWrapper(mean=[0.485, 0.456, 0.406],std=[0.229, 0.224, 0.225])
tens = ToTensor()
Document = namedtuple("Document", ["windows", "proposals", "identifier"])

class InferenceLoader(XMLLoader):

    def __init__(self, session, ingest_objs, classes):
        super().__init__(session, ingest_objs, classes)

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

#    def __len__(self):
#        return self.ndocs
#
    def __getitem__(self, item):
        example = super(InferenceLoader, self).__getitem__(item)
        uuid = self.uuids[item]
        ex_db = get_example_for_uuid(uuid, self.session)
        return example, ex_db

#
#
#    def _slices(self, img, proposals):
#        proposals_lst = proposals.tolist()
#        windows = []
#        for proposal in proposals_lst:
#            img_sub = img.crop(proposal)
#            img_sub = img_sub.resize((self.warped_size, self.warped_size))
#            img_data = tens(img_sub)
#            img_data = normalizer(img_data)
#            windows.append(img_data)
#        return torch.stack(windows)
