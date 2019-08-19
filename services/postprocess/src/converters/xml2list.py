"""
from ...converters.xml2list import xml2list
convert VOC XML to a list
"""
import numpy as np
from xml.etree import ElementTree as ET


def mapper(obj):
    """
    map a single object to the list structure
    :param obj: an Etree node
    :return: (type, (x1, y1, x2, y2), score)
    """
    diff = obj.find("difficult")
    score = float(diff.text) if diff is not None else 0
    bnd = obj.find("bndbox")
    coords = ["xmin", "ymin", "xmax", "ymax"]
    return obj.find("name").text, [int(float(bnd.find(coord).text)) for coord in coords], score


def test_merge_below():
    parameter = [('x', (10, 10, 40, 200)), ('x', (15, 190, 45, 220))]
    expected = [('x', (10, 10, 45, 220))]
    actual = merge_below(parameter)
    if expected == actual:
        print('Test 1 passes')
    else:
        print('Test 1 fails')
    parameter = [('x', (10, 10, 40, 200))]
    expected = [('x', (10, 10, 40, 200))]
    actual = merge_below(parameter)
    if expected == actual:
        print('Test 2 passes')
    else:
        print('Test 2 fails')

    parameter = []
    expected = []
    actual = merge_below(parameter)

    if expected == actual:
        print('Test 3 passes')
    else:
        print('Test 3 fails')


def feather_list(objs, feather_x=2, feather_y=2, max_x=1920, max_y=1920):
    """
    Feather the input coordinates by some amount
    :param objs: [(t, coords)]
    :param feather_x: Feather x by this much
    :param feather_y: Feather y by this much
    :param max_x: Document X
    :param max_y: Document Y
    :return: [(t, feathered_coords, score)]
    """
    new_objs = []
    for obj in objs:
        t, coords, score = obj
        new_coords = [max(coords[0]-feather_x, 0), max(coords[1]-feather_y, 0),
                      min(coords[2]+feather_x, max_x), min(coords[3]+feather_y, max_y)]
        new_objs.append((new_coords, t, score))
    return new_objs


def xml2list(fp, tres=0, feather=False):
    """
    convert VOC XML to a list
    :param fp: file path to VOC XML file
    :param tres: score treshold to filter on
    :param feather: add a bit of feathering (improves OCR quality downstream)
    :return: [(type,(x1, y1, x2, y2),score)]
    """
    tree = ET.parse(fp)
    root = tree.getroot()
    objects = root.findall("object")
    lst = [mapper(obj) for obj in objects]
    print(lst)
    new_lst = [l for l in lst]
    feathered_new_lst = None
    if feather:
        feathered_new_lst = feather_list(new_lst)
    else:
        feathered_new_lst = new_lst
    feathered_new_lst.sort(key=lambda x: x[1])
    return feathered_new_lst


if __name__ == '__main__':
    test_merge_below()


