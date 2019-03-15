"""
Dataset class for training an
Image embedding
Author: Josh McGrath
"""
from torch.utils.data import Dataset
from collections import namedtuple
import torch
from torch_model.train.data_layer.xml_loader import XMLLoader, Example
from ingestion.ingest_images import get_example_for_uuid
import random


class ImageEmbeddingDataset(XMLLoader):
    def __init__(self, session, ingest_objs,classes):
        """
        Create an image embedding db
        :param db: a sqlalchemy session to query for images
        """
        super(ImageEmbeddingDataset, self).__init__(session, ingest_objs, classes)


    @staticmethod
    def collate(batch):
        return XMLLoader.collate(batch)

    def __getitem__(self, item):
        if item < len(self.uuids):
            item =  XMLLoader.__getitem__(self, item)
            item.label = torch.ones(1)
        # If item is out of index, grab a random obj and generate a negative sample
        uuid = random.choice(self.uuids)
        ex = get_example_for_uuid(uuid, self.session)
        label = torch.zeros(1)
        # We pass False here, which generates negative neighbors
        neighbors = ex.neighbors(False, self.uuids, self.session)
        neighbor_boxes = [n.bbox for n in neighbors]
        neighbor_windows = [n.window for n in neighbors]
        return Example(ex.bbox, label, ex.window, neighbor_boxes, neighbor_windows)

    def __len__(self):
        return 2 * len(self.uuids)
