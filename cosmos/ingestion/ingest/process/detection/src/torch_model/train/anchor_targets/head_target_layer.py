"""
Module which takes the RPN output
and Generates Region Of Interest Proposals
Author: Josh McGrath
"""
import torch
from torch import nn
from torch.nn import CrossEntropyLoss
from torch_model.utils.generate_anchors import generate_anchors
from torch_model.utils.matcher import match


class HeadTargetLayer(nn.Module):
    def __init__(self, ncls=1):
        super(HeadTargetLayer, self).__init__()
        self.ncls = ncls
        print(f"there are {ncls} classes")
        self.cls_loss = CrossEntropyLoss(reduction="mean")

    def forward(self, cls_scores, gt_clses, device):
        """
        process proposals from the RPN
        :param rois : [N x L x 4]
        :param cls_scores: [N x L X C ] of scores not probabilities for C classes and L rois per image
        :param gt_boxes: [M x 4] [x1, y1, x2, y2]
        :param gt_cls:[Mx1] cls_idx
        :return:
        """
        loss = self.cls_loss(cls_scores, gt_clses)
        return loss

