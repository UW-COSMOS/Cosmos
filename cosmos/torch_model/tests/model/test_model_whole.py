from model.model import MMFasterRCNN
from train.data_layer.xml_loader import XMLLoader
from model.utils.config_manager import ConfigManager
from PIL import Image
from torchvision.transforms import ToTensor
import torch
from os.path import join
import unittest


class TestModel(unittest.TestCase):

    def setUp(self):
        data_dir = "../data"
        self.xml_dir = join(data_dir, "annotations")
        self.img_dir = join(data_dir, "images")
        self.proposal_dir = join(data_dir, "proposals")
        self.img_type = "jpg"
        self.host = "localhost"
        self.cfg = ConfigManager("../data/model_config.yaml")
        self.device = torch.device("cpu")

    def test_init(self):
        model = MMFasterRCNN(self.cfg)
        self.assertIsNotNone(model)

    def test_forward_precomputed(self):
        model = MMFasterRCNN(self.cfg)
        loader = XMLLoader(img_dir=self.img_dir,
                           xml_dir=self.xml_dir,
                           proposal_dir=self.proposal_dir,
                           warped_size=self.cfg.CC_LAYER.WARPED_SIZE,
                           img_type=self.img_type,
                           host=self.host)
        pt = loader[0]
        ex = pt.ex_window
        proposal = pt.ex_proposal
        out = model(ex.unsqueeze(0), proposal)
        self.assertIsNotNone(out)
