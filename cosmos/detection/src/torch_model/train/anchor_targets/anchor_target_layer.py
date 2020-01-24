"""
Module which takes the RPN output
and Generates Region Of Interest Proposals
Author: Josh McGrath
"""
import torch
from torch import nn
from torch.nn import BCEWithLogitsLoss
from utils.matcher import match, NEGATIVE
from utils.generate_anchors import generate_anchors
from train.losses.smooth_l1_loss import SmoothL1Loss
from utils.boundary_utils import cross_boundary



class AnchorTargetLayer(nn.Module):
    def __init__(self, ratios, scales, image_size=1920, upper=0.4, lower=0.1, sample_num=256):
        super(AnchorTargetLayer, self).__init__()
        self.feat_stride = None
        self.ratios = ratios
        self.scales = scales
        self.image_size = image_size
        self.upper = upper
        self.lower = lower
        self.sample_num = sample_num
        self.anchors = None
        self.cls_loss = BCEWithLogitsLoss(reduction="mean")
        self.bbox_loss = SmoothL1Loss(0.1)

    def forward(self, cls_scores, bbox_deltas, gt_boxes, device):
        """
        process proposals from the RPN
        :param bbox_deltas: [N x 4K x H x W ]
        :param cls_scores: [N x 2K x H x W  ] of scores not probabilities
        :param gt_boxes: [M x 4] [x1, y1, x2, y2]
        :return:
        """

        """
        Algorithm
        1) get all center points
        2) make all anchors using center points
        3) apply bbox_deltas
        4) calculate IoUs
        5) find positive labels
        6) find negative labels
        7) sample down the negative labels
        8) calculate losses
        """
        # ensure center and original anchors have been precomputed
        if self.feat_stride is None:
            self.feat_stride = round(self.image_size / float(cls_scores.size(3)))
        if self.anchors is None:
            self.anchors = generate_anchors(self.feat_stride,
                                            cls_scores.size(3),
                                            self.ratios,
                                            self.scales).to(device)

        N, _, H, W = cls_scores.shape
        cls_scores = cls_scores.permute(0,2, 3, 1)
        # apply bbox deltas but first reshape to (batch,H,W,4K)
        bbox_deltas = bbox_deltas.permute(0, 2, 3, 1)
        # reshape again to match anchors (N,H,W,Kx4)
        bbox_deltas = bbox_deltas.reshape(N, H, W, -1, 4)
        _anchors = self.anchors.float()
        regions = _anchors + bbox_deltas
        # now we clip the boxes to the image
        regions = regions.view(N, -1, 4, H, W).permute(0, 3, 4, 1, 2)
        # reshape to [batch x L x 4]
        regions = regions.reshape(N, -1, 4)
        # filter the cross boundary images in training
        for i in range(N):
            regions[i, :, :] = cross_boundary(regions[i, :, :], self.image_size, device, remove=False)
        # we need anchors to be [L x 4]
        _anchors = _anchors.reshape(-1,4)
        #get matches/ losses per batch
        cls_scores = cls_scores.reshape(N, -1, 1)
        tot_cls_loss = 0.0
        tot_bbox_loss = 0.0
        tot_fg = 0.0
        tot_bg = 0.0
        for i in range(N):
            matches = match(regions[i, :, :], gt_boxes[i][:, :4].squeeze(0), self.upper, self.lower, device)
            # filter out neither targets
            pos_mask = matches >= 0
            pos_inds = pos_mask.nonzero()
            neg_mask = matches == NEGATIVE
            neg_inds = neg_mask.nonzero()
            # sample 256 anchors
            pos_inds = pos_inds.reshape(-1)
            npos = min(pos_inds.size(0), 128)
            pos_inds_perm = torch.randperm(pos_inds.size(0))[:npos]
            pos_inds = pos_inds[pos_inds_perm]
            bg_num = self.sample_num - npos
            bg_num = min(60, bg_num)
            perm = torch.randperm(neg_inds.size(0))
            sample_neg_inds = perm[:bg_num]
            sample_ned_inds = neg_inds[sample_neg_inds]
            gt_cls = torch.cat((torch.ones(pos_inds.size(0)), torch.zeros(sample_neg_inds.size(0)))).to(device)
            # grab cls_scores from each point
            pred_cls = torch.cat((cls_scores[i,pos_inds], cls_scores[i,sample_neg_inds])).to(device).squeeze()
            # TODO avoid this reshape edge case
            gt_cls = gt_cls.reshape(-1)
            pred_cls = pred_cls.reshape(-1)
            cls_loss = self.cls_loss(pred_cls, gt_cls.reshape(-1))
            if cls_loss != cls_loss:
                print(f"pred_cls: {pred_cls}")
                print(f"gt_cls: {gt_cls}")
            # we only do bbox regression on positive targets
            # get and reshape matches
            gt_indxs = matches[pos_inds].long()
            sample_gt_bbox = gt_boxes[i][:,gt_indxs, :]
            sample_gt_bbox = sample_gt_bbox.reshape(-1,4)
            sample_pred_bbox = regions[i,pos_inds, :]
            sample_roi_bbox = _anchors[pos_inds, :]            
            norm = torch.tensor(N).float()
            bbox_loss = self.bbox_loss(sample_pred_bbox, sample_gt_bbox,sample_roi_bbox, norm)
            tot_cls_loss = tot_cls_loss + cls_loss
            tot_bbox_loss = tot_bbox_loss + bbox_loss
            tot_fg += npos
            tot_bg += bg_num
        return tot_cls_loss, tot_bbox_loss, tot_bg, tot_fg, pred_cls.mean()

