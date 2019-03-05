from model.connected_components.cc_layer import  CCLayer
from model.utils.config_manager import ConfigManager
from torchvision.transforms import ToTensor, ToPILImage
from PIL import Image
import unittest
import torch


class TestCCLayer(unittest.TestCase):
    def setUp(self):
        self.cfg = ConfigManager("../data/model_config.yaml")
        self.image = Image.open("../data/images/2009_001444.jpg")
        transform = ToTensor()
        self.image = transform(self.image)
        self.proposals = [[1,1,344,388]]
        self.warped_size = self.cfg.CC_LAYER.WARPED_SIZE
        self.device = torch.device("cpu")

    def test_init(self):
        CC = CCLayer(self.cfg)
        self.assertIsNotNone(CC)

    def test_forward_precomputed(self):
        CC = CCLayer(self.cfg)
        out, proposals = CC(self.image,self.device, self.proposals)
        self.assertIsInstance(out, torch.Tensor)
        self.assertEqual(3, out.shape[1])
        self.assertEqual(out.shape[2], self.warped_size)
        self.assertEqual(out.shape[3], self.warped_size)
        pil = ToPILImage()
        img = pil(out.squeeze(0))
        img.save("out_warped_precomputed.jpg", "JPEG")
