"""
Multi Modal Faster-RCNN implementation
Author: Josh McGrath
"""
import torch
from torch import nn
from model.layers.featurization import Featurizer
from model.head.object_classifier import MultiModalClassifier
from model.utils.config_manager import ConfigManager
from model.utils.shape_utils import get_shape_info
class MMFasterRCNN(nn.Module):
    def __init__(self, cfg):
        """
        Initialize a MMFasterRCNN network
        :param cfg: configuration file path for the network, see model configs
        """
        super(MMFasterRCNN, self).__init__()
        cfg = ConfigManager(cfg)
        self.featurizer = Featurizer(cfg)
        N, H, W, D = get_shape_info(self.featurizer.backbone, (1, 3, cfg.CC_LAYER.WARPED_SIZE, cfg.CC_LAYER.WARPED_SIZE))
        self.head = MultiModalClassifier(H,
                                         W,
                                         D,
                                         cfg.HEAD.DIM,
                                         len(cfg.CLASSES))
        self.cls_names = cfg.CLASSES


    def forward(self, *inputs, **kwargs):
        """
        Process an Image through the network
        """
        maps, proposals = self.featurizer(*inputs, **kwargs)
        cls_scores = self.head(maps)
        return proposals,  cls_scores


    def set_weights(self,mean, std):
        for child in self.children():
            if child == self.featurizer:
                continue
            for parm in self.modules():
                if not hasattr(parm, "weight"):
                    continue
                w = parm.weight
                # skip pretrained layers
                if w.requires_grad:
                    nn.init.normal_(w,mean, std)
                    if hasattr(parm, "bias") and not (parm.bias is None) :
                        nn.init.constant_(parm.bias, 0)

                




