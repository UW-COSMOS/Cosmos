"""
Multi Modal Faster-RCNN implementation
Author: Josh McGrath
"""
import torch
from torch import nn
from .attention.embedder import ImageEmbedder
from .attention.transformer import MultiHeadAttention
from .layers.featurization import Featurizer
from .head.object_classifier import MultiModalClassifier
from .utils.config_manager import ConfigManager
from .utils.shape_utils import get_shape_info
class MMFasterRCNN(nn.Module):
    def __init__(self, cfg):
        """
        Initialize a MMFasterRCNN network
        :param cfg: configuration file path for the network, see model configs
        """
        super(MMFasterRCNN, self).__init__()
        cfg = ConfigManager(cfg)
        self.featurizer = Featurizer(cfg)
        N, H, W, D = get_shape_info(self.featurizer.backbone, (1, 3, cfg.WARPED_SIZE, cfg.WARPED_SIZE))
        self.attention = MultiHeadAttention(cfg.NHEADS, cfg.EMBEDDING_DIM)
        self.embedder = Im
        self.head = MultiModalClassifier(H,
                                         W,
                                         D,
                                         cfg.HEAD_DIM,
                                         len(cfg.CLASSES))
        self.cls_names = cfg.CLASSES


    def forward(self, input_windows,neighbor_windows, proposals, device):
        """
        Process an Image through the network
        """
        maps = self.featurizer(input_windows, device)
        V = self.featurizer(neighbor_windows)
        Q = I
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

                




