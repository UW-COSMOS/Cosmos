"""
Visualize an xml file (not by class, but by cc) primarily for debugging cc
"""

from converters.xml2list import xml2list
from connected_components.connected_components import draw_cc
import click
import glob
import os
from PIL import Image
import numpy as np

@click.command()
@click.argument('xml_dir')
@click.argument('img_dir')
@click.argument('output_dir')
def visualize_xml(xml_dir, img_dir, output_dir):
    for xml in glob.glob(os.path.join(xml_dir, '*.xml')):
        bname = os.path.basename(xml)[:-4]
        png_name = os.path.join(img_dir, bname + '.png')
        img = Image.open(png_name)
        img_np = np.array(img.convert('RGB'))
        xlist = xml2list(xml)
        x_coords_list = [x[1] for x in xlist]
        out_path = os.path.join(output_dir, bname + '.png')
        draw_cc(img_np, x_coords_list, write_img_p=out_path)


if __name__ == '__main__':
    visualize_xml()


