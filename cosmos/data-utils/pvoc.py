"""
Turn a PAGE dataset into a PASCAL VOC dataset
"""
from pad_image import pad_image
from argparse import ArgumentParser
import os
import random
from xml_utils import parse_xml
from PIL import  Image
from tqdm import tqdm
parser = ArgumentParser(description="turn a PAGE dataset into a PASCAL VOC dataset")
parser.add_argument("input_path", type=str, help="a path to the PAGE dataset")
parser.add_argument("output_path", type=str, help="the path to output a PASCAL VOC datset under")
parser.add_argument("frac", type=float, help="the fraction of data to be used in the train set")
parser.add_argument("shape", type=int, help="the desired side length of an image")
def page_to_voc(input_path, output_path, frac, shape):
    generate_directories(output_path)
    generate_split(input_path, output_path, frac)
    transform_place_annotations(input_path, output_path, shape)
    transform_place_images(input_path, output_path, shape)

"""
Generate directory structure

output_path/
    Annotations/ 
        ...
    ImageSets/
        ...
    JPEGImages/
        ...
"""

def generate_directories(output_path):
    print("generating directories")
    try:
        os.mkdir(output_path)
        paths = ["Annotations", "ImageSets", "JPEGImages","ImageSets/Main"]
        for path in paths:
      	  os.mkdir("{}/{}".format(output_path, path))
    except FileExistsError:	
        print("ERROR: directory {} already exists".format(output_path))
        exit(1)



def get_image_identifiers(input_path):
    file_names = os.listdir("{}/Annotations".format(input_path))
    image_identifiers = [os.path.splitext(name)[0] for name in file_names]
    return image_identifiers
"""
perform train, evaluate split
and generate the appropriate imageset 
text files containing image identifiers
"""
def generate_split(input_path,output_path ,frac):
    """
    :param output_path: the base path of the VOC dataset
    :param frac: the fraction of images which should be in the train set
    :return: None
    """
    print("generating split")
    image_identifiers = get_image_identifiers(input_path)
    random.shuffle(image_identifiers)
    index = round(frac*len(image_identifiers))
    index = int(index)
    train = image_identifiers[0:index]
    evaluate = image_identifiers[index:]
    train_handle = open("{}/ImageSets/Main/train.txt".format(output_path), "w+")
    for name in train:
        train_handle.write("{}\n".format(name))
    train_handle.close()
    evaluate_handle = open("{}/ImageSets/Main/val.txt".format(output_path), "w+")
    for name in evaluate:
        evaluate_handle.write("{}\n".format(name))
    evaluate_handle.close()


"""
Change images from bitmaps to JPEGs
and place them in the PASCAL VOC directory
"""
def transform_place_images(input_path, output_path, shape):
    """

    :param output_path: the base path of the VOC dataset
    :param input_path: the base path to the PAGE dataset
    :return: None
    """
    print("padding images")
    identifiers = get_image_identifiers(input_path)
    for identifier in tqdm(identifiers):
        path = "{}/Image/{}.jpg".format(input_path, identifier)
        im = Image.open(path)
        im = pad_image(im , shape)
        out = "{}/JPEGImages/{}.jpg".format(output_path, identifier)
        im.save(out, "jpeg")


"""
Transform PAGE annotations to PASCAL VOC annotations
and write them to the correct directory
"""
def transform_place_annotations(input_path, output_path, shape):
    """
    :param output_path:
    :param input_path:
    :return:
    """
    image_identifiers = get_image_identifiers(input_path)
    base_path = "{}/Annotations".format(input_path)
    image_path = "{}/Image".format(input_path)
    output_dir = "{}/Annotations".format(output_path)
    for identifier in image_identifiers:
        image_out = "{}/JPEGImages/{}.jpg".format(output_path, identifier)
        parse_xml(base_path, identifier, image_path,image_out,output_dir, shape)


if __name__ == "__main__":
    args = parser.parse_args()
    page_to_voc(args.input_path, args.output_path, args.frac, args.shape)
