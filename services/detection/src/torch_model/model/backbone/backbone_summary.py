from torchsummary import summary
import torchvision.models as models
from torch import nn
import torch
base = models.resnet50(pretrained=True)
base = nn.Sequential(*list(base.children())[:-2])
summary(base, (3, 250, 250))
print(base(torch.rand((1,3,500,500))).shape)