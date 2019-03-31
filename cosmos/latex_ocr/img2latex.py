import sys, os

sys.path.append(os.path.dirname(__file__))
import random
import re
from scipy.misc import imread
import PIL
from PIL import Image
import os
import uuid
from imgaug import augmenters as iaa
from model.img2seq import Img2SeqModel
from model.utils.general import Config, run
from model.utils.text import Vocab
from model.utils.image import (
    greyscale,
    crop_image,
    pad_image,
    downsample_image,
    TIMEOUT,
)
from imgaug import augmenters as iaa
import os
import click
import tensorflow as tf


def img2latex(
    model,
    img,
    downsample_image_ratio=1,
    cropping=False,
    padding=False,
    img_augment=None,
    gray_scale=True,
):
    """
    Predict a latex code for an input equation image.
    :param model: model to be used
    :param img: input equation image
    :param downsample_image_ratio: down sampling ratio
    :param cropping: whether to crop
    :param padding: whether to pad
    :param img_augment: img augmentation filter
    :param gray_scale:whether to gray scale
    :return: latex prediction, processed img, processed img location
    """
    dir_output = "tmp/"
    run(["mkdir -p tmp"], TIMEOUT)
    name = str(uuid.uuid4())
    img_path = os.path.join("tmp/", f"{name}.png")
    img.save(img_path)
    buckets = [
        [240, 100],
        [320, 80],
        [400, 80],
        [400, 100],
        [480, 80],
        [480, 100],
        [560, 80],
        [560, 100],
        [640, 80],
        [640, 100],
        [720, 80],
        [720, 100],
        [720, 120],
        [720, 200],
        [800, 100],
        [800, 320],
        [1000, 200],
        [1000, 400],
        [1200, 200],
        [1600, 200],
        [1600, 1600],
    ]

    img_path_tmp = dir_output + "{}.png".format(name)

    if cropping:
        crop_image(img_path, img_path_tmp)

    if padding:
        pad_image(img_path_tmp if cropping else img_path, img_path_tmp, buckets=buckets)

    if downsample_image_ratio != 1:
        if cropping or padding:
            downsample_image(img_path_tmp, img_path_tmp, ratio=downsample_image_ratio)
        else:
            downsample_image(img_path, img_path_tmp, ratio=downsample_image_ratio)

    if cropping or padding or downsample_image_ratio != 1:
        img = imread(img_path_tmp)
    else:
        img = imread(img_path)

    if img_augment:
        img = img_augment.augment_image(img)

    img_obj = Image.fromarray(img)
    img_obj.save(img_path_tmp)

    if gray_scale:
        last = greyscale(img)
    else:
        last = img

    hyps = model.predict(last)

    return hyps[0], img, os.path.abspath(img_path_tmp)


def pdf2latex(model, pdf_path):
    """
    Make prediction for PDF
    :param model: model to be used
    :param pdf_path: PDF location
    :return:
    """
    buckets = [
        [240, 100],
        [320, 80],
        [400, 80],
        [400, 100],
        [480, 80],
        [480, 100],
        [560, 80],
        [560, 100],
        [640, 80],
        [640, 100],
        [720, 80],
        [720, 100],
        [720, 120],
        [720, 200],
        [800, 100],
        [800, 320],
        [1000, 200],
        [1000, 400],
        [1200, 200],
        [1600, 200],
        [1600, 1600],
    ]

    dir_output = "tmp/"
    name = pdf_path.split("/")[-1].split(".")[0]
    run(
        "magick convert -density {} -quality {} {} {}".format(
            200, 100, pdf_path, dir_output + "{}.png".format(name)
        ),
        TIMEOUT,
    )
    pdf_path = dir_output + "{}.png".format(name)
    crop_image(pdf_path, pdf_path)
    pad_image(pdf_path, pdf_path, buckets=buckets)
    downsample_image(pdf_path, pdf_path, 2)

    img = imread(pdf_path)

    img = greyscale(img)
    hyps = model.predict(img)

    # model.logger.info(hyps[0])

    return hyps[0], pdf_path


def easiest_latex_fix_from_left(tokens):
    """
    Fix imbalance brackets iterating from left
    :param tokens: List of tokens
    :return: Fixed sequence
    """
    c = 0
    for w in tokens:
        if w == "{":
            c += 1
            yield w
        elif w == "}":
            if c == 0:
                continue
            else:
                c -= 1
                yield w
        else:
            yield w


def easiest_latex_fix_from_right(tokens):
    """
    Fix imbalance brackets iterating from right
    :param tokens: List of tokens
    :return: Fixed sequence
    """
    c = 0
    for w in tokens[::-1]:
        if w == "{":
            if c == 0:
                continue
            c -= 1
            yield w
        elif w == "}":
            c += 1
            yield w
        else:
            yield w


def remove_bad_underscore(tokens):
    """
    Fix invalid underscore sequence
    :param tokens: List of tokens
    :return: Fixed sequence
    """
    merged = "".join(tokens)
    merged = re.sub(r"[_]{2,}", "_", merged)
    merged = merged.replace("}_}", "}}")
    merged = merged.replace("{_{", "{{")
    merged = re.sub(r"^_", "", merged)
    merged = re.sub(r"_$", "", merged)
    merged = re.sub(r"[_]{2,}", "_", merged)
    return list(merged)


def remove_bad_camma(tokens):
    """
    Remove invalid camma
    :param tokens: List of tokens
    :return: Fixed sequence
    """
    merged = "".join(tokens)
    merged = re.sub(r"\\,", "", merged)
    return merged


def strip(tokens, forbidden=[]):
    """
    Remove unnecessary command
    :param tokens: List of tokens
    :param forbidden command to be removed
    :return: Fixed sequence
    """
    merged = "".join(tokens)
    for cmd in forbidden:
        merged = re.sub(cmd.replace("\\", "\\\\"), "", merged)
    return list(merged)


def replace_empty_bracket(tokens):
    """
    Remove empty bracket
    :param tokens: List of tokens
    :return: Fixed sequence
    """
    merged = "".join(tokens)
    find = re.search(r"\{\}", merged)
    while find:
        merged = re.sub(r"\{\}", "", merged)
        find = re.search(r"\{\}", merged)
    return list(merged)


def postprocess(raw_latex):
    """
    Wrapper function for performing different postprocess operations
    :param raw_latex: latex code
    :return: processed latex code
    """
    tokens = raw_latex.split()
    recorded_command = list(filter(lambda x: "\\" in x, tokens))
    tokens = strip(tokens, ["\\mathrm", "\\Big", "\\cal"])
    tokens = remove_bad_underscore(tokens)
    tokens = remove_bad_camma(tokens)
    tokens = replace_empty_bracket(tokens)
    # print(tokens)
    tokens = list(easiest_latex_fix_from_left(tokens))
    # print(''.join(tokens))
    tokens = reversed(list(easiest_latex_fix_from_right(tokens)))
    # print(''.join(tokens))
    merged = "".join(tokens)

    # add space after commands
    for cmd in recorded_command:
        merged = merged.replace(cmd, cmd + " ")
    return merged


def get_im2latex_model(weight_dir):
    """
    Load up model from the given weight location
    :param weight_dir: weight location
    :return: trained model
    """
    os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
    tf.logging.set_verbosity(tf.logging.ERROR)
    config_vocab = Config(weight_dir + "vocab.json")
    config_model = Config(weight_dir + "model.json")
    vocab = Vocab(config_vocab)
    model = Img2SeqModel(config_model, weight_dir, vocab)
    model.build_pred()
    model.restore_session(weight_dir + "model.weights/")

    return model


def img2latex_api(model, img, downsample_image_ratio, cropping, padding, gray_scale):
    """
       Predict a latex code for an input equation image.
       :param model: model to be used
       :param img: input equation image
       :param downsample_image_ratio: down sampling ratio
       :param cropping: whether to crop
       :param padding: whether to pad
       :param gray_scale:whether to gray scale
       :return: latex prediction, processed img, processed img location
   """
    seq = iaa.Sequential([iaa.GammaContrast(2)])
    latex, _, _ = img2latex(
        model,
        img,
        downsample_image_ratio=downsample_image_ratio,
        cropping=cropping,
        padding=padding,
        img_augment=seq,
        gray_scale=gray_scale,
    )
    processed_latex = postprocess(latex)
    return processed_latex


# downsample_image_ratio=1, cropping=False, padding=False, img_augment=None, gray_scale=True
@click.command()
@click.option("--downsample_image_ratio", default=2, help="Ratio to down sampling")
@click.option("--cropping", default=True, help="Crops the source image")
@click.option("--padding", default=True, help="Pads the source image")
@click.option("--gray_scale", default=True, help="Gray scales the source image")
@click.option(
    "--weight_dir",
    required=True,
    help="Path to configuration folder under which there're vocab.json model.json model.weights",
)
@click.option("--img_path", required=True, help="Path to source img")
def img2latex_cli(
    weight_dir, img_path, downsample_image_ratio, cropping, padding, gray_scale
):
    """Program that takes as input an image of equation and outputs a Latex code"""
    img = Image.open(img_path)
    model = get_im2latex_model(weight_dir)
    processed_latex = img2latex_api(
        model, img, downsample_image_ratio, cropping, padding, gray_scale
    )
    click.echo(processed_latex)


if __name__ == "__main__":
    os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
    tf.logging.set_verbosity(tf.logging.ERROR)
    img2latex_cli()
