from torch_model.model.model import MMFasterRCNN
from torch_model.model.utils.config_manager import ConfigManager
from PIL import Image
from torchvision.transforms import ToTensor
import torch
from os.path import join
import unittest


class TestModel(unittest.TestCase):

    def setUp(self):
        self.cfg = ConfigManager("model_config.yaml")
        self.device = torch.device("cpu")

    def test_init(self):
        model = MMFasterRCNN(self.cfg)
        self.assertIsNotNone(model)


if __name__ == "__main__":
  unittest.main()
