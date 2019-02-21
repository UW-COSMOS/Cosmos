"""
Class to build and manage featurization backbones
Author: Josh McGrath
"""
from torch import nn
from model.roi.roi_align import ROIAlign
from model.rpn.rpn import RPN
from model.connected_components.cc_layer import CCLayer
from model.proposal.proposal_layer import ProposalLayer
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
        if self.method == "RPN":
            self.RPN = RPN(cfg.BACKBONE_DEPTH, cfg.RPN.DIM, cfg.RATIOS, cfg.SCALES)
            self.ROI_Align = ROIAlign(cfg.ROI_ALIGN.OUTPUT_SIZE,
                                      cfg.ROI_ALIGN.SPATIAL_SCALE,
                                      cfg.ROI_ALIGN.SAMPLING_RATIO)
            self.proposal_layer = ProposalLayer(cfg.RATIOS,
                                                cfg.SCALES,
                                                cfg.IMAGE_SIZE,
                                                cfg.NMS_PRE,
                                                cfg.NMS_POST,
                                                cfg.MIN_SIZE,
                                                cfg.NMS_THRESHOLD)

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
        feature_map = self.backbone(imgs)
        cls_branch_preds, cls_branch_scores, bbox_branch_preds = self.RPN(feature_map)
        if self.training:
            self.RPN_history = (cls_branch_preds, cls_branch_scores, bbox_branch_preds)
        proposals = self.proposal_layer(cls_branch_scores, bbox_branch_preds, device)
        windows = self.ROI_Align(proposals)
        return windows, proposals

    def _forward_CC(self, img_windows,device, proposals=None):
        windows = self.backbone(img_windows)
        return windows, proposals

    def get_RPN_outputs(self):
        return self.RPN_history
