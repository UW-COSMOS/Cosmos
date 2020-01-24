"""
Script to generate a VOC file from a PAGE record
Written by Josh McGrath
"""
import argparse
from pascal_voc_writer import Writer
import xml.etree.ElementTree as ET
from PIL import Image

def parse_xml(base_path, document,image_path ,image_out, output_dir, shape):
    """
        The page records for ICDAR are stored in an improper
        XML format, so the python library won't read it
        a fix is just to remove the first line
    """
    tree = ET.parse("{}/{}.xml".format(base_path,document))
    bit_path = "{}/{}.bmp".format(image_path,document)
    writer = Writer(image_out, shape, shape)
    root = tree.getroot()
    for el in root:
        class_name = el.tag
        for subel in el:
            coords = get_voc_coords(subel.attrib["points"])
            writer.addObject(class_name, *coords,difficult=1)
    writer.save("{}/{}.xml".format(output_dir, document))


def get_voc_coords(string):
    """
    convert PAGE record string coords into VOC coords
    :param string: a string containing the coordinates in PAGE format
    for example '168,146 476,146 168,326 476,326'
    :return: a tuple of (xmin, ymin,xmax, ymax)
    """
    points = string.split()
    points = [point.split(",") for point in points]
    x_points = [int(point[0]) for point in points]
    y_points = [int(point[1]) for point in points]
    return min(x_points),min(y_points), max(x_points), max(y_points)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="generate VOC files from PAGE files")
    parser.add_argument("base_path", type=str, help="base path to PAGE files")
    parser.add_argument("image_path", type=str, help="base path to bitmap files")
    parser.add_argument("document", type=str, help="path to PAGE record")
    parser.add_argument("output_dir", type=str, help="output directory")
    args = parser.parse_args()
    parse_xml(args.base_path, args.document, args.image_path, args.output_dir)
