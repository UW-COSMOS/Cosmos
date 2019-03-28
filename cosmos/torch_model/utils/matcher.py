from .bbox_overlaps import bbox_overlaps
import torch
NEGATIVE = -1
NEITHER = -2

def match(regions, gt_boxes, device=torch.device("cpu")):
    """
    Get positive region indexes for each box
    :param regions: predicted regions [KA x 4]
    :param gt_boxes: [ M x 4]
    :return: [KA x 4] index to gt box of boxes which have either
        a) an IOU of 0.7 or greater with a gt_box
        b) the highest IOU for a given gt_box


    """
    # get ious for each predicted box and gt target
    # get back an NxM tensor of IOUs
    overlaps = bbox_overlaps(regions, gt_boxes, device)

    assert overlaps.shape[0] == regions.shape[0]
    assert overlaps.shape[1] == gt_boxes.shape[0]
    
    # now we need the highest iou wrt to each
    # get back a vector indexed by gt_boxes which we need to
    # index back to targets
    best_score_pred, match_idxs_pred = torch.max(overlaps, dim=1)
    mask = best_score_pred <= 0.6
    match_idxs_pred[mask] = -1
    assert match_idxs_pred.shape[0] == regions.shape[0]

    return match_idxs_pred

