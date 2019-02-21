"""
Multimodal Network Head Implementation
of Faster RCNN
Author: Josh McGrath
"""

import torch
from torch import nn
from torch.nn.functional import relu, softmax
X = 0
Y = 1
X2 = 2
Y2 = 3

class MultiModalClassifier(nn.Module):
    def __init__(self, pool_height, pool_width, pool_depth, intermediate, ncls):
        """
        Initialize a Head object
        :param pool_height: The height of the output ROI pool
        :param pool_width: the width of the output ROI pool
        :param pool_depth: the depth of the output ROI pool
        :param intermediate: the dimensionality of the intermediate FC layer
        :param ncls: the number of classes
        """
        super(MultiModalClassifier, self).__init__()
        self.height = pool_height
        self.width = pool_width
        self.depth = pool_depth
        self.dropout = nn.Dropout(p=0.3)
        self.FC = nn.Linear(self.height*self.width*self.depth, intermediate)
        self.FC_2 = nn.Linear(intermediate, intermediate)
        self.cls_branch = nn.Linear(intermediate, ncls)


    def forward(self, roi_maps, proposals=None):
        """

        :param roi_maps: [NxLxDHxW]
        :return:
        """
        N, D, H, W = roi_maps.shape
        x = roi_maps.view(N, self.depth * self.width * self.height)
        x = self.FC(x)
        x = self.dropout(x)
        x = relu(x)
        x = self.FC_2(x)
        x = self.dropout(x)
        x = relu(x)
        cls_scores = self.cls_branch(x)
        return cls_scores

