from cosmos.torch_model.train.data_layer.xml_loader import XMLLoader
import unittest
from os.path import join
import torch
from utils.voc_utils import ICDAR_convert

class TestXMLLoader(unittest.TestCase):
    def setUp(self):
        data_dir = "../data"
        self.xml_dir = join(data_dir, "annotations")
        self.img_dir = join(data_dir, "images")
        self.proposal_dir = join(data_dir, "proposals")
        self.img_type = "png"
        self.host="localhost"
        self.warped_size = 300

    def test_init(self):
        loader = XMLLoader(self.img_dir,
                           self.xml_dir,
                           self.proposal_dir,
                           self.warped_size,
                           self.img_type,
                           self.host)

        self.assertIsNotNone(loader)

    def test_load_img_shape(self):
        loader = XMLLoader(self.img_dir,
                           self.xml_dir,
                           self.proposal_dir,
                           self.warped_size,
                           self.img_type,
                           self.host)
        pt = loader[0]
        self.assertIsInstance(pt.ex_window, torch.Tensor)
        self.assertEqual(pt.ex_window.shape[2], self.warped_size)
        self.assertEqual(pt.ex_window.shape[0], 3)

    def test_load_gt(self):
        loader = XMLLoader(self.img_dir,
                           self.xml_dir,
                           self.proposal_dir,
                           self.warped_size,
                           self.img_type,
                           self.host)
        pt = loader[0]

        self.assertIsInstance(pt.ex_window, torch.Tensor)
        self.assertEqual(pt.gt_box.shape[0], 4)


    def load_no_gt(self):
        loader = XMLLoader(self.img_dir,
                           xml_dir=None,
                           proposal_dir=self.proposal_dir,
                           warped_size=self.warped_size,
                           img_type=self.img_type,
                           host=self.host)
        res = loader[0]
        self.assertEqual(len(res), 4)



