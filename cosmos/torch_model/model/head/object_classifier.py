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
    """ Head classifier """
    def __init__(self, pool_height, pool_width, pool_depth, intermediate,nheads,  ncls):
        """
        Initialize a Head object
        :param pool_height: The height of the output ROI pool
        :param pool_width: the width of the output ROI pool
        :param pool_depth: the depth of the output ROI pool
        :param intermediate: the dimensionality of the intermediate FC layer
        :param ncls: the number of classes
        """
        super(MultiModalClassifier, self).__init__()
        self.nheads = nheads
        self.height = pool_height
        self.width = pool_width
        self.depth = pool_depth
        self.dropout = nn.Dropout(p=0.5)
        self.intermediate = int(intermediate)
        self.FC = nn.Linear(self.height*self.width*self.depth,self.intermediate)
        self.attn_FC = nn.Linear(self.height*self.width*self.depth, self.intermediate)
        #one extra for colorfulness
        self.FC_2 = nn.Linear(1+self.intermediate*(nheads+1), self.intermediate)
        self.cls_branch = nn.Linear(self.intermediate, ncls)


    def forward(self, roi_maps,attn_maps, colors,proposals=None):
        """

        :param roi_maps: [NxLxDHxW]
        :return:
        """
        N, D, H, W = roi_maps.shape
        x = roi_maps.view(N, self.depth * self.width * self.height)
        attn_maps = attn_maps.view(N, self.nheads, self.depth * self.width *self.height)
        attn_processed = []
        for i in range(self.nheads):
            sub_maps = attn_maps[0,i]
            processed = self.attn_FC(sub_maps)
            processed.reshape(-1)
            attn_processed.append(processed)
        attn_processed = torch.stack(attn_processed)
        x = self.FC(x)
        x = torch.cat((x, attn_processed))
        x = x.view(1,(self.nheads+1)*self.intermediate)
        x = torch.cat((x, colors), dim=1)
        x = self.dropout(x)
        x = relu(x)
        x = self.FC_2(x)
        x = self.dropout(x)
        x = relu(x)
        cls_scores = self.cls_branch(x)
        return cls_scores

