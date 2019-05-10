import unittest
from cosmos.torch_model.model.model import MMFasterRCNN
from cosmos.torch_model.model.utils.config_manager import ConfigManager
from cosmos.torch_model.inference.inference import InferenceHelper
from cosmos.torch_model.inference.data_layer.inference_loader import InferenceLoader
import torch
class TestInference(unittest.TestCase):
    def setUp(self):
        self.img_dir = "../data/images"
        self.proposals_dir = "../data/proposals"
        self.img_type = "png"
        config_path = "../data/model_config.yaml"
        cfg =  ConfigManager(config_path)
        self.model = MMFasterRCNN(config_path)
        self.model.eval()
        self.model.load_state_dict(torch.load("../data/weights/weights.pth", map_location={"cuda:0":"cpu"}))
        self.loader = InferenceLoader(img_dir=self.img_dir,
                                      img_type=self.img_type,
                                      proposal_dir=self.proposals_dir,
                                      warped_size=cfg.CC_LAYER.WARPED_SIZE)


    def test_init(self):
        helper = InferenceHelper(self.model, self.loader, torch.device("cpu"))
        self.assertIsNotNone(helper)

    def test_runs(self):
        helper = InferenceHelper(self.model, self.loader, torch.device("cpu"))
        helper.run("predictions")
        self.assertIsNotNone(helper)

