"""
Project images into an embedding space
For use in a Transformer Network
Author: Josh McGrath
"""


import torch
from torch import nn
from torch.nn.functional import relu
class ImageEmbedder(nn.Module):
    def __init__(self,conv_size,conv_depth, intermediate_d, out_d):
        super(ImageEmbedder, self).__init__()       
        print("super called")
        print(f"going to build a {conv_size*conv_size*conv_depth} by {intermediate_d} matrix of weights")
        self.FC = nn.Linear(conv_size*conv_size*conv_depth, intermediate_d)
        self.out = nn.Linear(intermediate_d, out_d)

    def forward(self, maps):
        N, _, _, _ = maps.shape
        x = maps.view(N, -1)
        x = self.FC(x)
        x = relu(x)
        x = self.out(x)
        return x
