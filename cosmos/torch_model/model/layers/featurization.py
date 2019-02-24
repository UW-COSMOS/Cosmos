"""
Class to build and manage featurization backbones
Author: Josh McGrath
"""
from torch import nn

from model.connected_components.cc_layer import CCLayer
from model.backbone.backbone import get_backbone
class Featurizer(nn.Module):

    def __init__(self, cfg):
        """
        Initialize a featurizer layer
        :param cfg: A model config to read from
        """
        super(Featurizer, self).__init__()
        self.backbone = get_backbone(cfg.BACKBONE)
        self.method = cfg.PROPOSAL_METHOD
        self.RPN = None
        self.ROI_Align = None
        self.proposal_layer = None
        self.cc = None
        self.RPN_history = []
        if self.method == "CONNECTED_COMPONENTS":
            self.cc = CCLayer(cfg)

    def forward(self, *input,**kwargs):
        """
        Delegation function
        :param input:
        :return: [N x L x H x W x K], [N x L x 4] convolutional maps and locations
        """
        if self.method == "CONNECTED_COMPONENTS":
            windows = self._forward_CC(*input, **kwargs)
        else:
            windows = self._forward_RPN(*input)
        return windows

    def _forward_RPN(self, imgs, device):
        raise NotImplementedError()

    def _forward_CC(self, img_windows,device, proposals=None):
        windows = self.backbone(img_windows)
        return windows, proposals

    def get_RPN_outputs(self):
        return self.RPN_history
