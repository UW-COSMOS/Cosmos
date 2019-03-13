"""
Dataset class for training an
Image embedding
Author: Josh McGrath
"""
from torch.utils.data import Dataset
from collections import namedtuple
import torch
from torch_model.train.data_layer.xml_loader import XMLLoader, Example



class ImageEmbeddingDataset(XMLLoader):
    def __init__(self, session, ingest_objs):
        """
        Create an image embedding db
        :param db: a sqlalchemy session to query for images
        """
        super(ImageEmbeddingDataset, self).__init__(session, ingest_objs)


    @staticmethod
    def collate(batch):
        return XMLLoader.collate(batch)

    def __getitem__(self, item):
        if item < len(self.uuids):
            return XMLLoader.__getitem__(self, item)
        # If item is out of index, grab a random obj and generate a negative sample
        uuid = random.choice(self.uuids)
        ex = get_example_for_uuid(uuid, self.session)
        # We pass False here, which generates negative neighbors
        neighbors = ex.neighbors(False, self.uuids, self.session)
        neighbor_boxes = [n.bbox for n in neighbors]
        neighbor_windows = [n.window for n in neighbors]
        return Example(center_bb=ex.bbox, label=ex.label, center_window=ex.window, neighbor_boxes=neighbor_boxes, neighbor_windows=neighbor_windows)

    def __len__(self):
        return 2 * len(self.uuids)
