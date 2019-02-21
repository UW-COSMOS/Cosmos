"""
Module which takes the RPN output
and Generates Region Of Interest Proposals
Author: Josh McGrath
"""
import numpy as np
import torch
from torch import nn
from .generate_anchors import generate_anchors
from model.nms.nms_wrapper import nms


def clip_boxes(boxes, im_shape, batch_size):

    for i in range(batch_size):
        boxes[i,:,0::4].clamp_(0, im_shape[1]-1)
        boxes[i,:,1::4].clamp_(0, im_shape[0]-1)
        boxes[i,:,2::4].clamp_(0, im_shape[1]-1)
        boxes[i,:,3::4].clamp_(0, im_shape[0]-1)

    return boxes

def bbox_transform_inv(boxes, deltas, batch_size):
    # TODO make this support batching
    widths = boxes[:, :, 2] - boxes[:, :, 0] + 1.0
    heights = boxes[:, :, 3] - boxes[:, :, 1] + 1.0
    ctr_x = boxes[:, :, 0] + 0.5 * widths
    ctr_y = boxes[:, :, 1] + 0.5 * heights

    dx = deltas[:, :, 0::4]
    dy = deltas[:, :, 1::4]
    dw = deltas[:, :, 2::4]
    dh = deltas[:, :, 3::4]

    pred_ctr_x = dx * widths.unsqueeze(2) + ctr_x.unsqueeze(2)
    pred_ctr_y = dy * heights.unsqueeze(2) + ctr_y.unsqueeze(2)
    pred_w = torch.exp(dw) * widths.unsqueeze(2)
    pred_h = torch.exp(dh) * heights.unsqueeze(2)

    pred_boxes = deltas.clone()
    # x1
    pred_boxes[:, :, 0::4] = pred_ctr_x - 0.5 * pred_w
    # y1
    pred_boxes[:, :, 1::4] = pred_ctr_y - 0.5 * pred_h
    # x2
    pred_boxes[:, :, 2::4] = pred_ctr_x + 0.5 * pred_w
    # y2
    pred_boxes[:, :, 3::4] = pred_ctr_y + 0.5 * pred_h

    return pred_boxes

class ProposalLayer(nn.Module):
    def __init__(self, feat_stride, ratios, scales, image_size=1920, NMS_PRE=3000, NMS_POST=300, min_size=64, threshold=0.6):
        super(ProposalLayer).__init__()
        #TODO figure out how this is calculated
        self.feat_stride = feat_stride
        self.ratios = ratios
        self.scales = scales
        self.image_size = image_size
        self.NMS_PRE = NMS_PRE
        self.NMS_POST = NMS_POST
        self.threshold = threshold
        self.min_size = min_size
        self.anchors = torch.from_numpy(generate_anchors(scales=np.array(scales),ratios=np.array(ratios))).float()
        self.num_anchors = self.anchors(0)

    def forward(self, cls_scores, bbox_preds):
        """
        process proposals from the RPN
        :param bbox_preds: [N x K x H x W x 4 ]
        :param cls_scores: [N x K x H x W x 2 ] of scores not probabilities
        :return:
        """
        batch_size = bbox_preds.size(0)
        # drop bg probs
        cls_probs = cls_scores[:, :, :, :, 0]
        # get size of feature map
        map_h, map_w = cls_scores.size(2), cls_scores.size(3)
        # get shifts
        shifts_x = torch.arange(0, map_w) * self.feat_stride
        shifts_y = torch.arange(0, map_h) * self.feat_stride
        shifts_x, shifts_y = torch.meshgrid(shifts_x, shifts_y)
        shifts = torch.stack((shifts_x.flatten(), shifts_y.flatten(),
                              shifts_x.flatten(), shifts_y.flatten())).transpose(0,1)
        # Enumerate all shifted anchors:
        #
        # add A anchors (1, A, 4) to
        # cell K shifts (K, 1, 4) to get
        # shift anchors (K, A, 4)
        # reshape to (K*A, 4) shifted anchors
        A = self.num_anchors
        K = shifts.size(0)
        anchors = self._anchors.reshape((1, A, 4)) + \
                  shifts.reshape((1, K, 4)).transpose((1, 0, 2))
        anchors = anchors.reshape((K * A, 4))

        # Transpose and reshape predicted bbox transformations to get them
        # into the same order as the anchors:
        #
        # bbox deltas will be (1, 4 * A, H, W) format
        # transpose to (1, H, W, 4 * A)
        # reshape to (1 * H * W * A, 4) where rows are ordered by (h, w, a)
        # in slowest to fastest order
        bbox_deltas = bbox_preds.transpose((0, 2, 3, 1)).reshape((-1, 4))

        # Same story for the scores:
        #
        # scores are (1, A, H, W) format
        # transpose to (1, H, W, A)
        # reshape to (1 * H * W * A, 1) where rows are ordered by (h, w, a)
        scores = cls_scores.transpose((0, 2, 3, 1)).reshape((-1, 1))

        # Convert anchors into proposals via bbox transformations
        proposals = bbox_transform_inv(anchors, bbox_deltas, batch_size)
        # 2. clip predicted boxes to image
        proposals = clip_boxes(proposals, (self.image_size, self.image_size), batch_size)

        scores_keep = scores
        proposals_keep = proposals
        _, order = torch.sort(scores_keep, 1, True)

        output = scores.new(batch_size, self.NMS_POST, 5).zero_()
        for i in range(batch_size):
            # # 3. remove predicted boxes with either height or width < threshold
            # # (NOTE: convert min_size to input image scale stored in im_info[2])
            proposals_single = proposals_keep[i]
            scores_single = scores_keep[i]

            # # 4. sort all (proposal, score) pairs by score from highest to lowest
            # # 5. take top pre_nms_topN (e.g. 6000)
            order_single = order[i]

            if self.NMS_PRE > 0 and self.NMS_POST < scores_keep.numel():
                order_single = order_single[:self.NMS_POST]

            proposals_single = proposals_single[order_single, :]
            scores_single = scores_single[order_single].view(-1, 1)

            # 6. apply nms (e.g. threshold = 0.7)
            # 7. take after_nms_topN (e.g. 300)
            # 8. return the top proposals (-> RoIs top)

            keep_idx_i = nms(torch.cat((proposals_single, scores_single), 1), self.threshold, force_cpu=True)
            keep_idx_i = keep_idx_i.long().view(-1)

            if self.NMS_POST > 0:
                keep_idx_i = keep_idx_i[:self.NMS_POST]
            proposals_single = proposals_single[keep_idx_i, :]
            scores_single = scores_single[keep_idx_i, :]

            # padding 0 at the end.
            num_proposal = proposals_single.size(0)
            output[i, :, 0] = i
            output[i, :num_proposal, 1:] = proposals_single

        return output

