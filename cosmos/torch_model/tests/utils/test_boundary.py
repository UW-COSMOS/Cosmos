"""
test the module that filters bboxes
which cross the image boundary
Author: Josh McGrath
"""

import torch
from utils.boundary_utils import cross_boundary
# our image will be 500x500
img_size = 500
device = torch.device("cpu")
# create boxes with (center_x, center_y, height, width)
# this box will be in the upper left hand corner
box_outside_negative = torch.tensor([15, 15, 40, 40])
box_inside = torch.tensor([60, 60, 30, 30])
# this box will go out the lower right hand side
box_outside_positive = torch.tensor([400, 400, 250, 250])

bbox_coords = torch.stack((box_outside_negative, box_inside, box_outside_positive)).float()

print(cross_boundary(bbox_coords, img_size, device))