import argparse
from lxml import etree
from os import listdir, path
import re
from itertools import groupby

from Parser.preprocess import load_file_to_tree


def pagemerger(rawfolder, outputfolder):
    PAGENAME_NUMBER_PATTERN = re.compile("(.*)_([0-9]+).html")
    def get_filename(filename):
        page_match = PAGENAME_NUMBER_PATTERN.search(filename)
        return page_match.group(1) if page_match is not None else None

    def get_page_number(filename):
        page_match = PAGENAME_NUMBER_PATTERN.search(filename)
        return page_match.group(2) if page_match is not None else None


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

        with open(path.join(outputfolder, key.replace(' ', '_')) + '.html', 'wb') as f:
            f.write(etree.tostring(root, pretty_print=True))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--rawfolder', default='data/html/files')
    parser.add_argument('--outputfolder', default='data/html/merged')
    args = parser.parse_args()
    pagemerger(args.rawfolder, args.outputfolder)
