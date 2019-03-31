import argparse
from lxml import etree
from os import listdir, path
import sys, os
sys.path.append(os.path.dirname(__file__))
import re
from itertools import groupby

from parse_preprocess import load_file_to_tree


def pagemerger(rawfolder):
    """
    Group files by filename.
    :param rawfolder: Folder containing HTML files where each HTML file only represents a single HTML page.
    :return: Dictionary where key is the filename and value is the corresponding document tree.
    """
    PAGENAME_NUMBER_PATTERN = re.compile("(.*)-([0-9]+).html")
    def get_filename(filename):
        page_match = PAGENAME_NUMBER_PATTERN.search(filename)
        return page_match.group(1) if page_match is not None else None

    def get_page_number(filename):
        page_match = PAGENAME_NUMBER_PATTERN.search(filename)
        return page_match.group(2) if page_match is not None else None


    merged_files = {}

    for key, group in groupby(sorted(listdir(rawfolder)), key=get_filename):
        if group is None or key is None:
            continue

        html = etree.Element('html')
        root = etree.SubElement(html, 'body')

        for file in group:
            page = etree.SubElement(root, 'div', page=get_page_number(file))
            tree = load_file_to_tree(path.join(rawfolder, file))

            for element in tree[1]:
                page.append(element)

        merged_files[key.replace(' ', '_') + '.html'] = root
        #
        # with open(path.join(outputfolder, key.replace(' ', '_')) + '.html', 'wb') as f:
        #     f.write(etree.tostring(root, pretty_print=True))

    return merged_files


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--rawfolder', help='contains only the html source files and nothing else',
                        default='data/html/files')
    parser.add_argument('--outputfolder', help='intermediate folder that stores merged HTMLs',
                        default='data/html/merged')
    args = parser.parse_args()
    pagemerger(args.rawfolder, args.outputfolder)
