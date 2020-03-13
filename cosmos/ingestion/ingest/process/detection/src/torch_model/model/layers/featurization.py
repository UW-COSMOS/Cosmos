"""
Class to build and manage featurization backbones
Author: Josh McGrath
"""
from torch import nn

from ..connected_components.cc_layer import CCLayer
from ..backbone.backbone import get_backbone
class Featurizer(nn.Module):
    """ Featurizer class """

    def __init__(self, cfg):
        """
        Initialize a featurizer layer
        :param cfg: A model config to read from
        """
        super(Featurizer, self).__init__()
        self.backbone = get_backbone(cfg.BACKBONE)
        self.cc = CCLayer(cfg)

    def forward(self, *input,**kwargs):
        """
        Delegation function
        :param input:
        :return: [N x L x H x W x K], [N x L x 4] convolutional maps and locations
        """
        return self._forward_CC(*input, **kwargs)


    def _forward_CC(self, img_windows,device):
        windows = self.backbone(img_windows)
        return windows

    def get_RPN_outputs(self):
        return self.RPN_history
