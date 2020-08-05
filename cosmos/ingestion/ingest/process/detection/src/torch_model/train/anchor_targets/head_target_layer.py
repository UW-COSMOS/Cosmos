"""
Module which takes the RPN output
and Generates Region Of Interest Proposals
Author: Josh McGrath
"""
from torch import nn
from torch.nn import CrossEntropyLoss
import torch.nn.functional as F

class HeadTargetLayer(nn.Module):
    def __init__(self, ncls=1):
        super(HeadTargetLayer, self).__init__()
        self.ncls = ncls

    def forward(self, cls_scores, gt_clses, weights=None, return_num_correct=False):
        """
        Loss layer
        :param cls_scores: Prediction tensor
        :param gt_clses: Ground truth tensor
        :return: Class loss
        """
        loss = F.cross_entropy(cls_scores, gt_clses, weight=weights)
        return loss

