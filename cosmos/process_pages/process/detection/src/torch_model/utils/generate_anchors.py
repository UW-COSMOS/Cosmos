import numpy as np
import torch
from itertools import product

def generate_anchors(feat_stride, map_size, ratios, scales,output_centers=False):
    """
    generate the actual image space anchor points without bbox_deltas applied
    so they can be stored in memory
    :param feat_stride: the number of relative pixel shifts in image space that
     correspond to a single pixel shift in feature map space
    :param map_size: the side length of the feature map
    :param ratios: the ratios for each anchor
    :param scales: the scales for each anchor
    :return: [K x H x W x x4] K being the number of anchors
    """
    # first generate all center points [H x W x 2]
    center_pts = np.ones((map_size, map_size, 2))
    for i in range(map_size):
        for j in range(map_size):
            center = (feat_stride*i +feat_stride/2.0), (feat_stride*j +feat_stride/2.0 )
            center_pts[i, j] = np.array(center)
    # [Hx Wx K x4]
    anchors = np.ones((map_size, map_size, len(ratios)*len(scales), 4))
    for i in range(map_size):
        for j in range(map_size):
            x, y = center_pts[i, j]
            for idx, (ratio, scale) in enumerate(product(ratios, scales)):
                h = scale*ratio
                w = scale
                anchors[i, j, idx] = np.array((x, y, h, w))
    if output_centers:
        return center_pts, torch.from_numpy(anchors)
    return torch.from_numpy(anchors).requires_grad_(False)
