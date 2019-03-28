"""
Convert an html file back into an xml document
"""
from bs4 import BeautifulSoup
import re
import os
import glob
import codecs
from postprocess.postprocess import not_ocr
from pascal_voc_writer import Writer
from argparse import ArgumentParser


def iterate_and_update_writer(soup, writer):
    """
    Core function for outputting xml
    :param soup: BeautifulSoup representation of html doc
    :param writer: pascal_voc_writer.Writer object
    :return: Updated pascal_voc_writer.Writer object
    """
    for seg_type in soup.find_all('div', not_ocr):
        seg_class = " ".join(seg_type["class"])
        hocr = seg_type.find_next('div', 'hocr')
        if hocr is None:
            print(seg_type)
            raise Exception('Invalid div found. Please account for said div')
        score = hocr['data-score']
        coordinates = hocr['data-coordinates']
        coordinates = coordinates.split(' ')
        coordinates = [int(x) for x in coordinates]
        coordinates = tuple(coordinates)
        writer.addObject(seg_class, *coordinates, difficult=float(score))
    return writer


def htmlfile2xml(html_f_path, output_path):
    """
    Take as input an html file, and output back an xml from that representation
    :param html_f_path: Path to html file
    :param output_path: Path to output new xml
    """
    with codecs.open(html_f_path, "r", "utf-8") as fin:
        soup = BeautifulSoup(fin, 'html.parser')
        writer = Writer(f'{os.path.basename(html_f_path)[:-5]}.png', 1920, 1920)
        writer = iterate_and_update_writer(soup, writer)
        writer.save(f'{os.path.join(output_path, os.path.basename(html_f_path)[:-5])}.xml')


def html2xml(html_path, output_path):
    """
    Helper function to sequentially call htmlfile2xml
    :param html_path: path to html folder
    :param output_path: path to output folder
    """
    for f in glob.glob(os.path.join(html_path, "*.html")):
        htmlfile2xml(f, output_path)


if __name__ == '__main__':
    parser = ArgumentParser(description='Convert html output to xml output')
    parser.add_argument('htmldir', type=str, help='Path to html directory')
    parser.add_argument('outputdir', type=str, help='Path to output directory')
    args = parse_args()
    html2xml(args.htmldir, args.outputdir)

