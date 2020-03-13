"""
Mutli Head Attention Module
Implements the Transformer Network Architecture
Author: Josh McGrath
"""
import torch
from torch import nn
from torch.nn.functional import softmax

# TODO this code does not yet support batching

class ScaledDotProductAttention(nn.Module):
    def __init__(self):
        super(ScaledDotProductAttention, self).__init__()

    def forward(self, Q, K, V):
        """
        Scaled Dot Product Attention for Images
        Q = [1 x Dims]
        K = [M x Dims]
        V = [M x H x W x 3]
        """
        neighbors, dim = K.shape
        logits = torch.matmul(Q, K.t())
        logits = torch.div(logits, dim)
        weights = softmax(logits, dim=1)
        weighted_V = []
        for idx in range(V.shape[0]):
            weighted_V.append(V[idx]*weights[0,idx])
        V = torch.stack(weighted_V)
        del weighted_V
        return torch.sum(V, dim=0)



class MultiHeadAttention(nn.Module):
    def __init__(self, nheads, emb_dim):
        super(MultiHeadAttention, self).__init__()
        self.attention = ScaledDotProductAttention()
        self.nheads = nheads
        self.emb_dim = emb_dim
        self.Q_heads = nn.ModuleList([nn.Linear(emb_dim, emb_dim) for i in range(nheads)])
        self.K_heads = nn.ModuleList([nn.Linear(emb_dim, emb_dim) for i in range(nheads)])

    def forward(self, Q, K,V):
        new_qs = [Q_head(Q) for Q_head in self.Q_heads]
        new_ks = [K_head(K) for K_head in self.K_heads]
        head_results = torch.stack([self.attention(new_qs[i], new_ks[i], V) for i in range(self.nheads)])
        return head_results
