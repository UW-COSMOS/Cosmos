"""
Convert a directory of PDFs to a directory
of XML annotations.
Author: Josh McGrath
"""
from tqdm import tqdm
from time import sleep
from os import mkdir, listdir
from subprocess import call
from glob import glob
from os.path import splitext, join
from multiprocessing import Pool
from PIL import ImageOps, Image
from functools import partial


def preprocess(input_dir, output_dir, args):
    """
    script entry point
    :param input_dir: Path to directory of PDFs
    :param output_dir: Desired output directory for XML
    :return:
    """
    # used to store intermediate PNGs

    try:
        mkdir(output_dir)
        pdf2png(input_dir, output_dir)
    except FileExistsError:
        print("output and tmp directories exist, overwriting...")
        sleep(2)
    pad_pngs(output_dir, args["padding"])



def pdf2png(input_dir, tmp_dir):
    """
    Convert PDFs into a directory of PNGs
    :param input_dir:
    :return:
    """
    # first we create all the directories we need
    files = glob(join(input_dir, "*.pdf"))

    def build_path(pdf_path):
        # take out input_dir
        pdf_path = pdf_path.split("/")[1]
        return splitext(pdf_path)[0]

    dir_names = map(build_path, files)
    # this is more parallelizable than we're taking advantage of
    for name in tqdm(dir_names):
        mkdir(join(tmp_dir, name))
        # now call ghostscript each file
        call(["./ghost.sh", name, f"{join(input_dir,name)}.pdf", tmp_dir])


def resize_png(path, size=1920):
    im = Image.open(path).convert('RGB')
    w, h = im.size
    if w >= size or h >= size:
        maxsize = (1920, 1920)
        im.thumbnail(maxsize, Image.ANTIALIAS)
    else:
        im = resize_image(im, size)
    return path,im

def pad_image(path, image=None, size=1920):
    im = Image.open(path).convert('RGB') if image is None else image
    w, h = im.size
    d_w = size - w
    d_h = size - h
    if d_h < 0 or d_w < 0:
        print(f'w: {w}, h: {h}')
        raise Exception("negative pad")
    padding = (0,0,d_w, d_h)
    im_2 = ImageOps.expand(im, padding, fill="#fff")
    return path,im_2


def resize_image(im, new_h):
    w,h = im.size
    if h > w:
        ratio = float(new_h)/h
        new_w = round(ratio*w)
    else:
        new_w = new_h
        ratio = float(new_w)/w
        new_h = round(ratio*h)
    im = im.resize((new_w, new_h), resample=Image.LANCZOS)
    return im


def pad_pngs(tmp_dir, padding):
    """
    pad a directory of images
    :param tmp_dir:
    :return:
    """
    pool = Pool(processes=4)
    dirs = listdir(tmp_dir)
    for dir in tqdm(dirs):
        pngs = glob(join(tmp_dir, dir, "*.png"))
        mapper = partial(pad_image, size=padding)
        images = pool.map(mapper, pngs)
        for path, image in tqdm(images):
            path = splitext(path)[0]
            image.save(f"{path}.jpg", "jpeg")




if __name__ == "__main__":
    args = {
        "padding": 1920,
        "collapse": False,
        "model_dir": "weights"
    }
    preprocess("data", "out", args)
