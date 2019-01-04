"""
convert VOC XML to a list
"""
from xml.etree import ElementTree as ET


def mapper(obj):
    """
    map a single object to the list structure
    :param obj: an Etree node
    :return: (type, (x1, y1, x2, y2))
    """
    bnd = obj.find("bndbox")
    coords = ["xmin", "ymin", "xmax", "ymax"]
    return obj.find("name").text, [int(float(bnd.find(coord).text)) for coord in coords]


def merge_below(objs, xtres=10):
    """
    This merge algo only merges objects that overlap a lot in the x axis and any amount in the y axis
    :param objs: list of (type, coords)
    :param xtres: Treshold for x buckets
    :return: list of (type, coords), merged
    """
    cls_map = {}
    for obj in objs:
        t, coords = obj
        if t in cls_map:
            cls_map[t].append(coords)
        else:
            cls_map[t] = [coords]
    final_list = []
    for cls in cls_map:
        coords_list = cls_map[cls]
        x_groupings = {}
        for coords in coords_list:
            xmin, xmax = coords[0], coords[2]
            ymin, ymax = coords[1], coords[3]
            found_f = False
            for grouping in x_groupings:
                gxmin, gxmax = grouping
                if gxmin - xtres <= xmin <= gxmin + xtres and gxmax - xtres <= xmax <= gxmax + xtres:
                    found_f = True
                    x_groupings[grouping].append(coords)
                    break
            if found_f is False:
                x_groupings[(xmin, xmax)] = [coords]
        for grouping in x_groupings:
            grouping_coords = x_groupings[grouping]
            # sort by ymin
            grouping_coords.sort(key=lambda x:x[1])
            curr_ymax = None
            merge_list = []
            in_merge_list = set()
            for ind, coords in enumerate(grouping_coords):
                if ind == 0:
                    curr_ymax = coords[3]
                    curr_ymax_ind = 0
                    continue
                ymin, ymax = coords[1], coords[3]
                # Overlap exists
                if ymin < curr_ymax < ymax:
                    merge_list.append((curr_ymax_ind, ind))
                    in_merge_list.add(curr_ymax_ind)
                    in_merge_list.add(ind)
                    curr_ymax = ymax
                    curr_ymax_ind = ind
                # No overlap, move curr pointers
                elif ymin > curr_ymax:
                    curr_ymax = ymax
                    curr_ymax_ind = ind
                # Box is subsumed
                if ymax < curr_ymax:
                    # Add it to the list of items to skip when handling non merges
                    in_merge_list.add(ind)
            for merge in merge_list:
                t, b = merge
                t_item = grouping_coords[t]
                b_item = grouping_coords[b]
                x1 = min(t_item[0], b_item[0])
                y1 = t_item[1]
                x2 = max(t_item[2], b_item[2])
                y2 = b_item[3]
                final_list.append((cls, (x1, y1, x2, y2)))
            for ind, coords in enumerate(grouping_coords):
                if ind not in in_merge_list:
                    final_list.append((cls, coords))
    return final_list


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


def feather_list(objs, feather_x=30, feather_y=10, max_x=1920, max_y=1920):
    """
    Feather the input coordinates by some amount
    :param objs: [(t, coords)]
    :param feather_x: Feather x by this much
    :param feather_y: Feather y by this much
    :param max_x: Document X
    :param max_y: Document Y
    :return: [(t, feathered_coords)]
    """
    new_objs = []
    for obj in objs:
        t, coords = obj
        new_coords = (max(coords[0]-feather_x, 0), max(coords[1]-feather_y, 0),
                      min(coords[2]+feather_x, max_x), min(coords[3]+feather_y, max_y))
        new_objs.append((t, new_coords))
    return new_objs


def xml2list(fp):
    """
    convert VOC XML to a list
    :param fp: file path to VOC XML file
    :return: [(type,(x1, y1, x2, y2))]
    """
    tree = ET.parse(fp)
    root = tree.getroot()
    objects = root.findall("object")
    lst =[mapper(obj) for obj in objects]
    new_lst = merge_below(lst)
    feathered_new_lst = feather_list(new_lst)
    feathered_new_lst.sort(key=lambda x: x[1])
    return feathered_new_lst


if __name__ == '__main__':
    test_merge_below()


