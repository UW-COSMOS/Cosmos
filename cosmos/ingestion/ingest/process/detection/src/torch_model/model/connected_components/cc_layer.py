import torch
from torch import nn
from .connected_components import get_components


class CCLayer(nn.Module):
    
    def __init__(self, cfg):
        super(CCLayer, self).__init__()
        self.warped_size = cfg.WARPED_SIZE

    def forward(self, img, device, proposals=None):
        """
        Delegation function
        :param img:
        :param device:
        :param proposals:
        :return:
        """
        if proposals is None:
            proposals = get_components(img)
        if len(proposals) > 1:
            raise ValueError("The CCLayer does not yet support batches greater than 1")
        proposals_lst = proposals[0]
        proposals_lst = proposals_lst.tolist()
        windows = [CCLayer.warp(img, p, self.warped_size, device) for p in proposals_lst]
        windows = [w.get() for w in windows]
        windows = torch.stack(windows).to(device)
        return windows, proposals

    def warp(img, proposal,warped_size, device):
        """
        warp an image to a fixed size
        :param img:
        :param proposal:
        :return:
        """
        from torchvision.transforms import ToPILImage, ToTensor
        pil = ToPILImage()
        ten = ToTensor()
        img = pil(img.squeeze(0))
        img = img.crop(proposal)
        img = img.resize((warped_size, warped_size))
        tens = ten(img)
        img.close()
        return tens
