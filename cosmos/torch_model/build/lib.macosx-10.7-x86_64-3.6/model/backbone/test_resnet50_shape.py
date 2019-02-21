from model.backbone.backbone import get_resnet
import numpy as np
import torch
backbone = get_resnet(50)
rdata = np.random.rand(1,3,1920,1920)
rdata = torch.from_numpy(rdata)
out = backbone(rdata.float())
print(out.shape)
