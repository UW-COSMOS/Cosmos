"""
Utilities for loading inference data into the model
"""
# TODO refactor so xml_loader and inference_loader import from a utilities directory
from train.data_layer.xml_loader import load_image, load_proposal
from torch.utils.data import Dataset
import torch
import os
from os.path import splitext
from torchvision.transforms import ToTensor
from train.data_layer.transforms import NormalizeWrapper
from collections import namedtuple

normalizer = NormalizeWrapper(mean=[0.485, 0.456, 0.406],std=[0.229, 0.224, 0.225])
tens = ToTensor()
Document = namedtuple("Document", ["windows", "proposals", "identifier"])

class InferenceLoader(Dataset):

    def __init__(self, img_dir, proposal_dir, img_type="jpg", warped_size=300):
        self.img_dir = img_dir
        self.proposal_dir = proposal_dir
        self.img_type = img_type
        self.imgs = os.listdir(img_dir)
        self.warped_size = warped_size
        self.identifiers = [splitext(img)[0] for img in self.imgs]
        self.ndocs = len(self.identifiers)

    @staticmethod
    def collate(batch):
        """
        collation function to be used with this dataset class
        :param batch:
        :return:
        """
        if len(batch) > 1:
            raise ValueError(f"Inference classes are only meant to be used with a batch size of 1, got {len(batch)}")

        return batch[0]

    def __len__(self):
        return self.ndocs

    def __getitem__(self, item):
        identifier = self.identifiers[item]
        img = load_image(self.img_dir, identifier, self.img_type)
        proposals = load_proposal(self.proposal_dir, identifier)
        windows = self._slices(img, proposals)
        doc = Document(windows, proposals, identifier)
        return doc


    def _slices(self, img, proposals):
        proposals_lst = proposals.tolist()
        windows = []
        for proposal in proposals_lst:
            img_sub = img.crop(proposal)
            img_sub = img_sub.resize((self.warped_size, self.warped_size))
            img_data = tens(img_sub)
            img_data = normalizer(img_data)
            windows.append(img_data)
        return torch.stack(windows)
