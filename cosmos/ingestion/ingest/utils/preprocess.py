"""
Preprocess fns for connected components
"""
from PIL import Image

def resize_png(im, return_size = False, size: int = 1920) -> Image:
    """
    Resize the PNG, but do not pad
    :param path: Path to png
    :param size: size to check to
    :return: path to image, Pil.Image
    """
    w, h = im.size
    if w >= size or h >= size:
        maxsize = (1920, 1920)
        im.thumbnail(maxsize, Image.ANTIALIAS)
    else:
        im = resize_image(im, size)
    if return_size:
        return im, im.size
    return im


def resize_image(im, new_h):
    w, h = im.size
    ratio = float(new_h)/h
    new_w = round(ratio*w)
    im = im.resize((new_w, new_h), resample=Image.LANCZOS)
    return im

