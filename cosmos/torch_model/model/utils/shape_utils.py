import torch

def get_shape_info(module, input_shape):
    """
    get the output shape of a module given a specific input shape
    :param module:
    :param input_shape:
    :return: the output shape of module(input)
    """
    x = torch.rand(input_shape)
    out = module(x)
    return out.shape