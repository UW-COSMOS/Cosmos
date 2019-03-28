import argparse
from lxml import etree
from os import listdir, path
import sys, os
sys.path.append(os.path.dirname(__file__))
import re
from itertools import groupby

from parse_preprocess import load_file_to_tree


def pagemerger(rawfolder, outputfolder):
    """
    Group files by filename.
    :param rawfolder: Folder containing HTML files where each HTML file only represents a single HTML page.
    :param outputfolder: Folder to store the merged HTML files.
    """
    PAGENAME_NUMBER_PATTERN = re.compile("(.*)_([0-9]+).html")
    def get_filename(filename):
        page_match = PAGENAME_NUMBER_PATTERN.search(filename)
        return page_match.group(1) if page_match is not None else None

    def get_page_number(filename):
        page_match = PAGENAME_NUMBER_PATTERN.search(filename)
        return page_match.group(2) if page_match is not None else None
    def get_page_int(filename):
        page_match = PAGENAME_NUMBER_PATTERN.search(filename)
        return int(page_match.group(2)) if page_match is not None else -1

    for name in sorted(listdir(rawfolder)):
        print(get_filename(name))
    for key, group in groupby(sorted(listdir(rawfolder)), key=get_filename):
        if group is None or key is None:
            continue

        html = etree.Element('html')
        root = etree.SubElement(html, 'body')

        for file in sorted(group, key=get_page_int):
            page = etree.SubElement(root, 'div', page=get_page_number(file))
            tree = load_file_to_tree(path.join(rawfolder, file))
            elem_dict = {}
            for element in tree[1]:
                children = element.xpath(".//*[@class='text_unicode']")
                if len(children) >= 1:
                    child = children[0]
                    idx = int(child.get('id'))
                    elem_dict[idx] = element
                else:
                    print('No text_unicode')
                    latexocr = element.xpath(".//*[@class='hocr_img2latex']")[0]
                    child = latexocr.getnext()
                    if child is not None:
                        print('The class is: '+child.get('class'))
                        child.set('class','text_unicode')
                        idx = int(child.get('id'))
                        elem_dict[idx] = element

            for elem_key in sorted(elem_dict.keys()):
                page.append(elem_dict[elem_key])

        with open(path.join(outputfolder, key.replace(' ', '_')) + '.html', 'wb') as f:
            f.write(etree.tostring(root, pretty_print=True))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--rawfolder', help='contains only the html source files and nothing else',
                        default='data/html/files')
    parser.add_argument('--outputfolder', help='intermediate folder that stores merged HTMLs',
                        default='data/html/merged')
    args = parser.parse_args()
    print(args.rawfolder)
    print(args.outputfolder)
    pagemerger(args.rawfolder, args.outputfolder)
