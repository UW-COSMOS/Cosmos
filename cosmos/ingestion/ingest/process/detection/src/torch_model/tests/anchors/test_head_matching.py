from utils.matcher import match
import torch

device = torch.device("cpu")
pred_regions = [[0, 0, 30, 30],
                [30, 30,  60, 60],
                [15, 15, 40, 40],
                [60, 60, 7, 7],
                [60, 60, 6, 6]]
gt_regions = [[0, 0, 30, 30],
              [10, 10, 40, 40],
              [60, 60, 10, 10]]

pred_regions = torch.tensor(pred_regions).float()
gt_regions = torch.tensor(gt_regions).float()
upper = 0.7
lower = 0.3

print(match(pred_regions, gt_regions, upper, lower, device))


# should get
"""
0
-1
1
2
-2
"""
