from bs4 import BeautifulSoup
import re
import os
import glob
import codecs
from evaluate.evaluate import calculate_iou

def not_ocr(text):
    """
    TODO: Docstring for not_ocr.

    Args:
        text (TODO): TODO

    Returns: TODO

    """
    return 'ocr' not in text and 'rawtext' not in text and 'unicode' not in text

def construct_hocr_list(soup):
    """
    Construct a list from the hocr input
    :param soup: BeautifulSoup input
    :return: [(class, coords)]
    """
    hocr_list = []
    for seg_type in soup.find_all('div', not_ocr):
        hocr = seg_type.find_next('div', 'hocr')
        if hocr is not None:
            coordinates = hocr['data-coordinates']
            spl = coordinates.split(' ')
            int_coords = [int(x) for x in spl]
            hocr_list.append((seg_type, int_coords))
    return hocr_list


def check_caption_body(soup):
    """
    Check for things that look like captions that are flagged as body test.

    Simply looks for "figure", "fig", "table", or "tab" plus a number at the start of a line

    Args:
        soup (bs4.BeautiulSoup): The page's html, in soup form

    Returns: Corrected soup
    """

    # Basic rule to set Page headers and footers
    hocr_list = construct_hocr_list(soup)
    hocr_list = sorted(hocr_list, key=lambda x: x[1][1])
    ph = None
    if len(hocr_list) > 0:
        ph = hocr_list[0][1]

    for seg_type in soup.find_all('div', not_ocr):
        seg_class = " ".join(seg_type["class"])
        hocr = seg_type.find_next('div', 'hocr')
        #if hocr is not None:
        #    coordinates = hocr['data-coordinates']
        #    spl = coordinates.split(' ')
        #    spl = [int(x) for x in spl]
        #    if spl == ph:
        #        seg_type["class"] = "Page Header"
        lines = seg_type.find_all('span', 'ocr_line')
        if len(lines) > 0:
            line = lines[0]
            clean_line = line.getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
            matches = re.findall('^(figure|fig)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
            if len(matches) >0:
                seg_type["class"] = "Figure Caption"
            matches = re.findall('^(table|tbl|tab)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
            if len(matches) >0 and seg_class != 'Table':
                seg_type["class"] = "Table Caption"

    return soup

def check_overlap(obj_list, box, check_above_below=False, check_cls=None):
    for cls, bb, _ in obj_list:
        iou = calculate_iou(bb, box)
        if check_cls is not None and iou > 0 and cls in check_cls:
            continue
        if check_above_below:
            intersection = max(0, min(bb[2], box[2]) - max(bb[0], box[0]))
            if intersection == 0:
                return False
        if iou == 0:
            continue
        return False
    return True


def group_cls_columnwise(obj_list, g_cls):
    """
    Given a list output from xml2list, group the class via columns
    :param obj_list: [(cls, coords, score)] list
    :return: Updated [(cls, coords, score)] list
    """
    nbhds = []
    for obj in obj_list:
        cls, coords, scr = obj
        if cls == g_cls:
            if len(nbhds) == 0:
                nbhds.append((coords, scr))
                continue
            new_nbhd_list = []
            added = False
            for nbhd_bb in nbhds:
                nbhd_bb, scr2 = nbhd_bb
                # construct a bounding box over this table and the neighborhood
                new_box = [min(nbhd_bb[0], coords[0]), min(nbhd_bb[1], coords[1]), max(nbhd_bb[2], coords[2]), max(nbhd_bb[3], coords[3])]
                if check_overlap(obj_list, new_box, check_above_below=True):
                    new_nbhd_list.append((new_box, scr if scr >= scr2 else scr2))
                    added = True
                else:
                    new_nbhd_list.append((nbhd_bb, scr2))
            # If we didn't merge with an existing neighborhood, create a new neighborhood
            if not added:
                new_nbhd_list.append((coords, scr))
            nbhds = new_nbhd_list
    new_obj_list = []
    for obj in obj_list:
        cls, coords, _ = obj
        if cls != g_cls:
            new_obj_list.append(obj)
    for nbhd in nbhds:
        new_obj_list.append((g_cls, nbhd, 1))
    return new_obj_list

def group_cls(obj_list, g_cls, do_table_merge=False, merge_over_classes=None):
    """
    Given a list output from xml2list, group the class in that list
    :param obj_list: [(cls, coords, score)] list
    """
    nbhds = []
    for obj in obj_list:
        cls, coords, scr = obj
        if cls == g_cls:
            if len(nbhds) == 0:
                nbhds.append((coords, scr))
                continue
            new_nbhd_list = []
            added = False
            for nbhd_bb in nbhds:
                # construct a bounding box over this table and the neighborhood
                nbhd_bb, scr2 = nbhd_bb
                new_box = [min(nbhd_bb[0], coords[0]), min(nbhd_bb[1], coords[1]), max(nbhd_bb[2], coords[2]), max(nbhd_bb[3], coords[3])]
                ccls = [g_cls]
                if merge_over_classes is not None:
                    ccls.extend(merge_over_classes)
                if check_overlap(obj_list, new_box, check_cls=ccls):
                    new_nbhd_list.append((new_box, scr if scr >= scr2 else scr2))
                    added = True
                else:
                    new_nbhd_list.append((nbhd_bb, scr2))
            # If we didn't merge with an existing neighborhood, create a new neighborhood
            if not added:
                new_nbhd_list.append((coords, scr))
            nbhds = new_nbhd_list
    if do_table_merge:
        while True:
            new_nbhds = []
            merged_nbhds = []
            # Now we check for intersecting table neighborhoods
            for nbhd in nbhds:
                nbhd, scr = nbhd
                for nbhd2 in nbhds:
                    nbhd2, scr2 = nbhd2
                    if nbhd2 == nbhd:
                        continue
                    iou = calculate_iou(nbhd, nbhd2)
                    if iou > 0:
                        # merge the neighborhoods
                        new_box = [min(nbhd[0], nbhd2[0]), min(nbhd[1], nbhd2[1]), max(nbhd[2], nbhd2[2]), max(nbhd[3], nbhd2[3])]
                        if new_box in new_nbhds:
                            continue
                        merged_nbhds.append(nbhd)
                        merged_nbhds.append(nbhd2)
                        new_nbhds.append((new_box, scr if scr >= scr2 else scr2))
            for nbhd in nbhds:
                nbhd, scr = nbhd
                if nbhd not in merged_nbhds:
                    new_nbhds.append((nbhd, scr))
            if new_nbhds == nbhds:
                break
            nbhds = new_nbhds

    filter_objs = []
    # now we need to check for other overlaps
    for nbhd in nbhds:
        nbhd, scr = nbhd
        for obj in obj_list:
            cls, coords, _ = obj
            obj_iou = calculate_iou(coords, nbhd)
            if obj_iou > 0:
                filter_objs.append(obj)
    obj_list = [o for o in obj_list if o not in filter_objs]
    new_obj_list = []
    for obj in obj_list:
        cls, coords, _ = obj
        if cls != g_cls:
            new_obj_list.append(obj)
    for nbhd in nbhds:
        nbhd, scr = nbhd
        new_obj_list.append((g_cls, nbhd, scr))
    return new_obj_list


def postprocess(html_path, output_path):
    """
    Postprocess entry point
    :param html_path: Path to html files
    :param output_path: Output path for postprocessing
    """
    for f in glob.glob(os.path.join(html_path, "*.html")):
        with codecs.open(os.path.join(output_path, os.path.basename(f)), "w", "utf-8") as fout:
            with codecs.open(f, "r", "utf-8") as fin:
                soup = BeautifulSoup(fin, 'html.parser')
                new_soup = check_caption_body(soup)
                print(f"Writing to {output_path}")
                fout.write(str(new_soup))

