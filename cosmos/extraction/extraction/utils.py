import torch
from torch.nn.utils.rnn import pad_sequence
from typing import Dict
CUDA = torch.cuda.is_available()
Encoding = Dict[str, torch.Tensor]

def remove_padding(xs: Encoding, pad_token) -> Encoding:
    mask = xs["input_ids"] != pad_token
    if mask.ndim == 1 or mask.shape[0] == 1:
        return {k: v[mask] for k, v in xs.items()}
    else:
        max_len = mask.sum(-1).max()
        return {k: v[:, :max_len] for k, v in xs.items()}


def collate(batch, padding_value=0):
    """simplified version of the pytorch default_collate_fn that deals with padding"""

    elem = batch[0]

    if isinstance(elem, torch.Tensor):
        return pad_sequence(batch, True, padding_value=padding_value)
    elif isinstance(elem, dict):
        return {k: collate([d[k] for d in batch], padding_value=padding_value) for k in elem}
    elif isinstance(elem, tuple):
        if type(elem) is not tuple:
            # elem is a NamedTuple
            cls = elem.__class__
            return cls(*(collate(samples, padding_value) for samples in zip(*batch)))
        else:
            return tuple(collate(samples, padding_value) for samples in zip(*batch))
    elif isinstance(elem, int):
        return torch.tensor(batch, dtype=torch.long).unsqueeze(-1)
    elif isinstance(elem, float):
        return torch.tensor(batch, dtype=torch.float32).unsqueeze(-1)
    else:
        raise TypeError(type(elem))
def cudafy(thing):
    if not CUDA:
        return thing
    if isinstance(thing, torch.Tensor):
        return thing.cuda()
    elif isinstance(thing, dict):
        return {k: cudafy(v) for k, v in thing.items()}
    elif isinstance(thing, tuple):
        cls = thing.__class__  # so this works on named tuples
        return cls(*(cudafy(t) for t in thing))
    elif isinstance(thing, list):
        return [cudafy(t) for t in thing]
    else:
        raise TypeError(type(thing))
