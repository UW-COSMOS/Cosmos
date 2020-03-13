"""
Multi Modal Faster-RCNN implementation
"""
import torch
from torch import nn
from .attention.embedder import ImageEmbedder
from .attention.transformer import MultiHeadAttention
from .layers.featurization import Featurizer
from .head.object_classifier import MultiModalClassifier
from .utils.config_manager import ConfigManager
from .utils.shape_utils import get_shape_info
class MMFasterRCNN(nn.Module):
    def __init__(self, cfg):
        """
        Initialize a MMFasterRCNN network
        :param cfg: configuration file path for the network, see model configs
        """
        super(MMFasterRCNN, self).__init__()
        cfg = ConfigManager(cfg)
        print("===== BUILDING MODEL ======")
        self.featurizer = Featurizer(cfg)
        print(f"Built backbone {cfg.BACKBONE}")
        print("Building downstream components via shape testing")
        N, D, H,W  = get_shape_info(self.featurizer.backbone, (1, 3, cfg.WARPED_SIZE, cfg.WARPED_SIZE))
        print("done shape testing, building, attention mechanisms")
        self.attention = MultiHeadAttention(cfg.NHEADS, cfg.EMBEDDING_DIM)
        print("built multi head attention")
        print(f"{H}, {W}, {D}")
        self.embedder = ImageEmbedder(H,D, cfg.EMBEDDING_INTERMEDIATE, cfg.EMBEDDING_DIM)
        print("built embeddings")
        self.head = MultiModalClassifier(H,
                                         W,
                                         D,
                                         cfg.HEAD_DIM,
                                         cfg.NHEADS,
                                         len(cfg.CLASSES))
        print("done")
        self.cls_names = cfg.CLASSES


    def forward(self, input_windows,neighbor_windows, radii, angles, colors,proposals, device):
        """
        Process an Image through the network
        :param input_windows: Tensor representing target window pixels
        :param neighbor_windows: Tensor representing neighbor window pixels
        :param radii: neighborhood embedding radii
        :param angles: neighborhood angle input
        :param colors: Color input
        :param proposals: proposals list
        :param device: Device config
        :return: proposals, associated class scores
        """
        maps = self.featurizer(input_windows, device)
        V = self.featurizer(neighbor_windows, device)
        Q = self.embedder(maps, torch.tensor([[0.0]]).to(device),torch.tensor([[0.0]]).to(device))
        K = self.embedder(V, radii, angles)
        attn_maps = self.attention(Q,K,V)
        cls_scores = self.head(maps, attn_maps,colors, proposals)
        return proposals, cls_scores

 
    def set_weights(self,mean, std):
        '''
        Set weights
        :param mean: weight mean
        :param std: weight standard deviation
        '''
        for child in self.children():
            if child == self.featurizer:
                continue
            for parm in self.modules():
                if not hasattr(parm, "weight"):
                    continue
                w = parm.weight
                # skip pretrained layers
                if w.requires_grad:
                    nn.init.normal_(w,mean, std)
                    if hasattr(parm, "bias") and not (parm.bias is None) :
                        nn.init.constant_(parm.bias, 0)

                




