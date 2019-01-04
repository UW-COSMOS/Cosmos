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
    lst.sort(key=lambda x: x[1])
    return lst


