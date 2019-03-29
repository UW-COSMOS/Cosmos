"""
Construct csvs for Figure--Figure Captions/ Tables -- Table Captions
"""

from lxml import etree
import os
from Parser.parse_preprocess import get_words_from_child as get_words
from converters.html2xml import htmlfile2xml
from converters.xml2list import xml2list
import pandas as pd
import multiprocessing as mp
import glob
from bs4 import BeautifulSoup
import codecs
from postprocess.postprocess import not_ocr
import click


def get_cls_list(html_f):
    """
    Given an html file, get a list of objects that's easier to reason about
    :param html_f: The input html file
    :return: [(cls, bb, score)]
    """
    htmlfile2xml(html_f, '/tmp')
    return xml2list(f'{os.path.join("/tmp", os.path.basename(html_f)[:-5])}.xml')


def get_target_map(html_f, target_cls, target_cls_association):
    """
    Get a map with targets and target associations
    :param html_f: html file to ingest
    :param target_cls: the target class
    :param target_cls_association: the target class association
    :return: dictionary mapping targets to target associations
    """
    cls_list = get_cls_list(html_f)
    cls_list = [(x[0], tuple(x[1]), x[2]) for x in cls_list]
    targets = [x for x in cls_list if x[0] == target_cls]
    if len(targets) == 0:
        return None
    cls_associations = [x for x in cls_list if x[0] == target_cls_association]
    target_map = {}
    for target in targets:
        if len(cls_associations) == 0:
            target_map[target] = None
            continue
        # compute two distances
        # 1) Distance from tl of target to bottom left of class associations
        # 2) Distance from br of target to top left of class associations
        cls_ass_bl = [(x[1][0], x[1][3]) for x in cls_associations]
        cls_ass_tr = [(x[1][2], x[1][1]) for x in cls_associations]
        def calc_tl_dists(x):
            bl_x, bl_y = x
            # squared dist
            return (target[1][0] - bl_x) ** 2 + (target[1][1] - bl_y) ** 2

        def calc_br_dists(x):
            tr_x, tr_y = x
            # squared dist
            return (target[1][2] - tr_x) ** 2 + (target[1][3] - tr_y) ** 2

        cls_ass_bl_dists = [calc_tl_dists(x) for x in cls_ass_bl]
        cls_ass_tr_dists = [calc_br_dists(x) for x in cls_ass_tr]
        bl_min_dist = min(cls_ass_bl_dists)
        tr_min_dist = min(cls_ass_tr_dists)
        cls_assoc = None
        if bl_min_dist <= tr_min_dist:
            ind = cls_ass_bl_dists.index(bl_min_dist)
            cls_assoc = cls_associations[ind]
        else:
            ind = cls_ass_tr_dists.index(tr_min_dist)
            cls_assoc = cls_associations[ind]
        target_map[target] = cls_assoc
    leftover = target_map.values()
    leftover_assocs = [assoc for assoc in cls_associations if assoc not in leftover]
    print(leftover_assocs)
    return target_map, leftover_assocs

def collect_words(xml_string, target):
    """
    Collect the words in an xml
    :param xml_string: xml string input
    :param target: Target class
    :return: String of word list
    """
    root = etree.fromstring(xml_string)
    word_list = [x['text'] for x in get_words(root, target)]
    return ' '.join(word_list)


def construct_single_df(html_f, target_cls, target_cls_association):
    """
    Construct a single df of target class and target_class association
    :param html_f: Path to html_file
    :param target_cls: Target class file
    :param target_cls_association: Association object
    :return: Df
    """
    target_map, leftover_assocs = get_target_map(html_f, target_cls, target_cls_association)
    if target_map is None:
        return None
    with codecs.open(html_f, 'r', 'utf-8') as f:
        soup = BeautifulSoup(f, 'html.parser')
        df_dict = {'target_img_path': [], 'target_unicode': [], 'target_tesseract': [], 'assoc_img_path': [], 'assoc_unicode': [], 'assoc_tesseract': []}
        for target in target_map:
            target_assoc = target_map[target]
            # The assumption made here is that we just need to extract text information from just the target
            # but we'll get the path to the image for both of them
            target_cls, target_bb, _ = target
            target_img_path = None
            target_unic = None
            target_tess = None
            for target_div in soup.find_all('div', target_cls):
                hocr = target_div.find_next('div', 'hocr')
                coordinates = hocr['data-coordinates']
                spl = coordinates.split(' ')
                spl = [int(x) for x in spl]
                spl = tuple(spl)
                if spl != target_bb:
                    continue
                img = target_div.find_next('img')
                target_img_path = str(img['src'])
                target_unic = str(target_div)#.find_next('div', 'text_unicode'))
                target_unic = collect_words(target_unic, 'text_unicode')
                target_tess = target_div.find_next('div', 'rawtext')
                target_tess = target_tess.text.strip()
                break
            # Sometimes there is no association to an object (Dangling caption).
            # TODO: Decide what to do in this case
            # For now, we will just add nans
            assoc_img_path = None
            assoc_unic = None
            assoc_tess = None
            if target_assoc is not None:
                assoc_cls, assoc_bb, _ = target_assoc
                for assoc_div in soup.find_all('div', assoc_cls):
                    hocr = assoc_div.find_next('div', 'hocr')
                    coordinates = hocr['data-coordinates']
                    spl = coordinates.split(' ')
                    spl = [int(x) for x in spl]
                    spl = tuple(spl)
                    if spl != assoc_bb:
                        continue
                    img = assoc_div.find_next('img')
                    assoc_img_path = str(img['src'])
                    assoc_unic = str(assoc_div)#.find_next('div', 'text_unicode'))
                    assoc_unic = collect_words(assoc_unic, 'text_unicode')
                    assoc_tess = assoc_div.find_next('div', 'rawtext')
                    assoc_tess = assoc_tess.text.strip()
                    break
            df_dict['target_img_path'].append(target_img_path)
            df_dict['assoc_img_path'].append(assoc_img_path)
            df_dict['target_unicode'].append(target_unic)
            df_dict['assoc_unicode'].append(assoc_unic)
            df_dict['target_tesseract'].append(target_tess)
            df_dict['assoc_tesseract'].append(assoc_tess)
        for assoc in leftover_assocs:
            assoc_cls, assoc_bb, _ = assoc
            for assoc_div in soup.find_all('div', assoc_cls):
                hocr = assoc_div.find_next('div', 'hocr')
                coordinates = hocr['data-coordinates']
                spl = coordinates.split(' ')
                spl = [int(x) for x in spl]
                spl = tuple(spl)
                if spl != assoc_bb:
                    continue
                img = assoc_div.find_next('img')
                assoc_img_path = str(img['src'])
                assoc_unic = str(assoc_div)#.find_next('div', 'text_unicode'))
                assoc_unic = collect_words(assoc_unic, 'text_unicode')
                assoc_tess = assoc_div.find_next('div', 'rawtext')
                assoc_tess = assoc_tess.text.strip()
                df_dict['target_img_path'].append(None)
                df_dict['assoc_img_path'].append(assoc_img_path)
                df_dict['target_unicode'].append(None)
                df_dict['assoc_unicode'].append(assoc_unic)
                df_dict['target_tesseract'].append(None)
                df_dict['assoc_tesseract'].append(assoc_tess)
                break
        df = pd.DataFrame(df_dict)
        df['html_file'] = os.path.basename(html_f)
        return df


def construct(html_dir, target_cls, assoc_cls, output_file, processes=160):
    """
    Construct the target <=> target association dataframe
    :param html_dir: Input html
    :param target_cls: Target class
    :param assoc_cls: Target association class
    :param output_file: Output path
    :param processes: Number of processes
    """
    pool = mp.Pool(processes=processes)
    ret = [pool.apply_async(construct_single_df, args=(f, target_cls, assoc_cls,)) for f in glob.glob(os.path.join(html_dir, '*.html'))]
    results = [r for r in results if r is not None]
    final_df = None
    if len(results) > 0:
        final_df = pd.concat(results)
    if final_df is None:
        print(f'{output_file} was not written as there were not any {target_cls} in the set of htmls')
        return
    final_df.to_csv(output_file, index=False)


@click.command()
@click.argument('html_dir')
@click.argument('target_cls')
@click.argument('assoc_cls')
@click.argument('output_file')
@click.option('--processes', help='Number of processes to spawn', default=160)
def construct_click(html_dir, target_cls, assoc_cls, output_file, processes):
    construct(html_dir, target_cls, assoc_cls, processes=processes)


if __name__ == '__main__':
    construct_click()



