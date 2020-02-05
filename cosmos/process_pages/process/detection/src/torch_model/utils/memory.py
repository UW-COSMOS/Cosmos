"""
memory debugging utilities
Author: Josh McGrath
"""

import bitmath
import torch

def format_mem(bytes):
    return bitmath.Byte(bytes).to_GiB()

def get_gpu_mem(gpu):
    mem = torch.cuda.memory_allocated(gpu)
    return format_mem(mem)
