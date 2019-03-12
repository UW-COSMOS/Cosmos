"""
Dataset class for training an
Image embedding
Author: Josh McGrath
"""
from torch.utils.data import Dataset
from collections import namedtuple
import torch



class ImageEmbeddingDataset(Dataset):
    def __init__(self, db):
        """
        Create an Image DB
        :param db: a sqlalchemy session to query for images
        """
        super(ImageEmbeddingDataset, self).__init__()
        self.db = db
        self.identifiers = []
        self._build()

    def _build(self):
        pass

    @staticmethod
    def collate(batch):
        words = torch.stack([item.word_window for item in batch])
        contexts = torch.stack([item.context_window for item in batch])
        word_bboxes = torch.stack([item.word_bbox for item in batch])
        context_bboxes = torch.stack([item.context_bbox for item in batch])
        labels = torch.stack([item.label for item in batch])
        return Batch(words, contexts, word_bboxes, context_bboxes, labels)

    def __getitem__(self, item):
        pass

    def __len__(self):
        return len(self.identifiers)
