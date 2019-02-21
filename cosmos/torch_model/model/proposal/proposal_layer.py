"""
Module which takes the RPN output
and Generates Region Of Interest Proposals
Author: Josh McGrath
"""

import torch
from torch import nn
from model.nms.nms import nms
from utils.generate_anchors import generate_anchors
from utils.boundary_utils import cross_boundary

def filter_regions(regions, min_size):
    """
    remove regions which are too small
    :param regions: [ Hx Wx K x4]
    :param min_size: integer, minimum length of a side
    :return: indexes of regions to be removed
    """
    # TODO implement, not currently implemented in matterport, or
    # in pytorch faster rcnn, or the old facebook implementation
    return regions



class ProposalLayer(nn.Module):
    def __init__(self, ratios, scales, image_size=1920, NMS_PRE=3000, NMS_POST=300, min_size=64, threshold=0.5):
        super(ProposalLayer,self).__init__()
        self.feat_stride = None
        self.ratios = ratios
        self.scales = scales
        self.image_size = image_size
        self.NMS_PRE = NMS_PRE
        self.NMS_POST = NMS_POST
        self.threshold = threshold
        self.min_size = min_size
        self.cpu = torch.device("cpu")
        self.anchors = None

    def forward(self, cls_scores, bbox_deltas, device):
        """
        process proposals from the RPN
        :param bbox_deltas: [N x 4K x H x W ]
        :param cls_scores: [N x 2K x H x W  ] of scores not probabilities
        :return:
        """

        """
        Algorithm
        1) get all center points
        2) make all anchors using center points
        3) apply bbox_deltas
        4) clip boxes to image
        5) filter small boxes
        6) pre NMS fitering by score
        7) NMS filtering
        8) post NMS filtering by score
        """
        # ensure center and original anchors have been precomputeds
        if self.feat_stride is None:
            self.feat_stride = round(self.image_size / float(cls_scores.size(3)))
        if self.anchors is None:
            self.anchors = generate_anchors(self.feat_stride,
                                            cls_scores.size(3),
                                            self.ratios,
                                            self.scales).to(device)
        N, _, H, W = cls_scores.shape
        cls_scores = cls_scores.permute(0, 2, 3, 1)

        # apply bbox deltas but first reshape to (0,2,3,1) = (12)(23)
        bbox_deltas = bbox_deltas.permute(0, 2, 3, 1)
        # reshape again to match anchors
        bbox_deltas = bbox_deltas.reshape(N, H, W, -1, 4)
        _anchors = self.anchors.float()
        regions = _anchors + bbox_deltas
        # now we clip the boxes to the image

        # now we can grab the pre NMS regions
        # first we reshape the tensors to be N x K, N x K x 4
        cls_scores = cls_scores.permute(0, 3, 1, 2).reshape(N, -1)
        regions = regions.view(N, -1, 4, H, W).permute(0, 3, 4, 1, 2)
        regions = regions.reshape(N,-1, 4)
        for i in range(N):
            regions[i, :, :] = cross_boundary(regions[i, :, :], self.image_size, device, remove=False)
        pre_nms = min(self.NMS_PRE, cls_scores.size(1))
        _, sort_order = cls_scores.topk(pre_nms, dim=1)
        slices_scores = []
        slices_regions = []
        for i in range(N):
            slice_idxs = sort_order[i,:]
            slice_score = cls_scores[i, slice_idxs]
            slice_region = regions[i, slice_idxs, :]
            slices_regions.append(slice_region)
            slices_scores.append(slice_score)
        cls_scores = torch.stack(slices_scores, dim=0)
        regions = torch.stack(slices_regions, dim=0)
        output = cls_scores.new(N, self.NMS_POST, 5)
        for i in range(N):
            keep_idx = nms(regions[i, :,:], cls_scores[i,:], self.threshold)
            keep_idx = keep_idx[:self.NMS_POST]
            output[i, :, 1:] = pad_tensor(regions[i, keep_idx, :], (self.NMS_POST,4))
            output[:, :, 0] = pad_tensor(cls_scores[i,keep_idx].unsqueeze(1), (self.NMS_POST,1)).squeeze()
        return output

    def backward(self, ctx, grad_output):
        pass


def pad_tensor(tens, size):
    ret = torch.zeros(size)
    ret[:tens.size(0),:] = tens
    return ret

