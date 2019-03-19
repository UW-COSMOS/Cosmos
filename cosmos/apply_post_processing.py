"""
Apply some postprocessing to an xml set
"""

import click
import postprocess.postprocess as post
import multiprocessing as mp
from converters.xml2list import xml2list
from converters.list2html import list2html
from converters.html2xml import htmlfile2xml
import os

def convert_to_html(xpath, img_dir):
    l = xml2list(xpath)
    print(f'{os.path.basename(xpath)[:-4]}.png')
    list2html(l, f'{os.path.basename(xpath)[:-4]}.png', img_dir, 'html2')

def update_xmls(hpath, output_dir):
    htmlfile2xml(hpath, output_dir)

@click.command()
@click.argument('xml_dir')
@click.argument('img_dir')
@click.argument('output_dir')
def run_xml_postprocessing(xml_dir, img_dir, output_dir):
    pool = mp.Pool(processes=160)
    results = [pool.apply_async(convert_to_html, args=(os.path.join(xml_dir, x), img_dir)) for x in os.listdir(xml_dir)]
    [r.get() for r in results]
    post.postprocess('html2', 'html3')
    results = [pool.apply_async(update_xmls, args=(os.path.join('html3', x), output_dir)) for x in os.listdir('html3') if x != 'img']
    [r.get() for r in results]


if __name__ == '__main__':
    run_xml_postprocessing()



