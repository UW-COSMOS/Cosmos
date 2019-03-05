"""
Region Proposal Network component of
Faster R-CNN

Author: Josh McGrath
"""
import torch
from torch import nn
import torch.nn.functional as F

class RPN(nn.Module):
    def __init__(self,input_depth,output_depth,ratios, scales, size=3):
        """
        Initialize a region Proposal network
        :param input_depth: number of filters coming out of the backbone
        :param size: window size of RPN (kernel)
        """
        super(RPN, self).__init__()
        stride = 1
        padding = 1
        bias = True
        self.num_anchors = len(ratios)*len(scales)
        self.RPN_conv = nn.Conv2d(input_depth,
                                  output_depth,
                                  size,
                                  stride,
                                  padding,
                                  bias=bias)
        #class branch weights
        # 2*num_anchors out
        self.ncls_out = self.num_anchors
        self.RPN_cls_score = nn.Conv2d(output_depth, self.ncls_out, 1, 1)
        self.RPN_cls_score.bias = torch.nn.Parameter(torch.ones(self.RPN_cls_score.bias.shape))
        self.nbbox_out = 4 * self.num_anchors
        self.RPN_bbox_pred = nn.Conv2d(output_depth, self.nbbox_out, 1, 1)


    def forward(self, feature_map):
        """
        process the feature map by slices
        :param feature_map:
        :return: [(objectness, [dx, dy, dw, dh])]
        """
        x = F.relu(self.RPN_conv(feature_map))
        cls_branch_scores = self.RPN_cls_score(x)
        """
        TODO make this something easy to iterate over in a left-right top-bottom
        fashion
        """
        cls_branch_preds = torch.sigmoid(cls_branch_scores)
        bbox_branch_preds = self.RPN_bbox_pred(x)
        return cls_branch_preds, cls_branch_scores, bbox_branch_preds
