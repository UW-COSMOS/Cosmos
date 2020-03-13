from cosmos.torch_model.inference.data_layer.inference_loader import InferenceLoader
import torch
from utils.bbox import BBoxes
import unittest


class TestInferenceLoader(unittest.TestCase):
    def setUp(self):
        self.img_dir = "../data/images"
        self.proposal_dir ="../data/proposals"
        self.img_type = "png"

    def test_init(self):
        dataset = InferenceLoader(img_dir=self.img_dir,
                                  proposal_dir=self.proposal_dir,
                                  img_type=self.img_type)
        self.assertEqual(3, len(dataset))

    def test_load_windows(self):
        dataset = InferenceLoader(img_dir=self.img_dir,
                                  proposal_dir=self.proposal_dir,
                                  img_type=self.img_type)
        doc = dataset[0]
        self.assertIsInstance(doc.windows, torch.Tensor)
        self.assertEqual(3, doc.windows.shape[1])
        self.assertEqual(dataset.warped_size, doc.windows.shape[2])
        self.assertEqual(dataset.warped_size, doc.windows.shape[3])

    def test_load_proposals(self):
        dataset = InferenceLoader(img_dir=self.img_dir,
                                  proposal_dir=self.proposal_dir,
                                  img_type=self.img_type)
        doc = dataset[0]
        self.assertIsInstance(doc.proposals, BBoxes)
        self.assertEqual(4, doc.proposals.shape[1])

    def test_load_joint(self):
        dataset = InferenceLoader(img_dir=self.img_dir,
                                  proposal_dir=self.proposal_dir,
                                  img_type=self.img_type)
        doc = dataset[0]
        self.assertEqual(doc.proposals.shape[0], doc.windows.shape[0])

