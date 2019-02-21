"""
Multimodal Network Head Implementation
of Faster RCNN
Author: Josh McGrath
"""

from torch import nn
from torch.nn.functional import relu, softmax

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
        super(MultiModalClassifier).__init__(self)
        self.height = pool_height
        self.width = pool_width
        self.depth = pool_depth
        self.FC = nn.Linear((self.height, self.width, self.depth), (intermediate,1))
        self.FC_2 = nn.Linear(intermediate, intermediate)
        self.cls_branch = nn.Linear(intermediate, ncls)
        self.bbox_branch = nn.Linear(intermediate, 4*ncls)


    def forward(self, roi_maps):
        """

        :param roi_maps: [NxHxWxD]
        :return:
        """
        #TODO support batching
        x = self.FC(roi_maps)
        x = relu(x)
        x = self.FC_2(x)
        x = relu(x)
        cls_scores = self.cls_branch(x)
        bbox_scores = self.bbox_branch(x)
        return softmax(cls_scores), cls_scores, bbox_scores


