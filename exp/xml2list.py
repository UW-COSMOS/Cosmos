"""
convert VOC XML to a list
"""
import numpy as np
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


def non_max_suppression_fast(boxes, overlapThresh):
    """
    Using Faster Non-Maximum Suppression, Malisiewicz et al.
    See: https://www.pyimagesearch.com/2015/02/16/faster-non-maximum-suppression-python/
    :param boxes: list of boxes
    :param overlapThresh: Overlapping threshold
    :return: List of merged boxes
    """
    if len(boxes) == 0:
        return []
    new_boxes = np.array([np.array(x) for x in boxes])

    # if the bounding boxes integers, convert them to floats --
    # this is important since we'll be doing a bunch of divisions
    if new_boxes.dtype.kind == "i":
        new_boxes = new_boxes.astype("float")

    # initialize the list of picked indexes    
    pick = []

    # grab the coordinates of the bounding boxes
    x1 = new_boxes[:,0]
    y1 = new_boxes[:,1]
    x2 = new_boxes[:,2]
    y2 = new_boxes[:,3]

    # compute the area of the bounding boxes and sort the bounding
    # boxes by the bottom-right y-coordinate of the bounding box
    area = (x2 - x1 + 1) * (y2 - y1 + 1)
    idxs = np.argsort(y2)

    # keep looping while some indexes still remain in the indexes
    # list
    while len(idxs) > 0:
        # grab the last index in the indexes list and add the
        # index value to the list of picked indexes
        last = len(idxs) - 1
        i = idxs[last]
        pick.append(i)

        # find the largest (x, y) coordinates for the start of
        # the bounding box and the smallest (x, y) coordinates
        # for the end of the bounding box
        xx1 = np.maximum(x1[i], x1[idxs[:last]])
        yy1 = np.maximum(y1[i], y1[idxs[:last]])
        xx2 = np.minimum(x2[i], x2[idxs[:last]])
        yy2 = np.minimum(y2[i], y2[idxs[:last]])

        # compute the width and height of the bounding box
        w = np.maximum(0, xx2 - xx1 + 1)
        h = np.maximum(0, yy2 - yy1 + 1)

        # compute the ratio of overlap
        overlap = (w * h) / area[idxs[:last]]

        # delete all indexes from the index list that have
        idxs = np.delete(idxs, np.concatenate(([last],
            np.where(overlap > overlapThresh)[0])))

    # return only the bounding boxes that were picked using the
    # integer data type
    final_boxes = new_boxes[pick].astype("int")
    return final_boxes.tolist()


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
    cl_map = {}
    for obj in lst:
        t, coords = obj
        if t not in cl_map:
            cl_map[t] = [coords]
        else:
            cl_map[t].append(coords)
    new_lst = []
    for key in cl_map:
        coords_list = cl_map[key]
        new_coords = non_max_suppression_fast(coords_list, 20)
        for coord in new_coords:
            new_lst.append((key, coord))

    new_lst.sort(key=lambda x: x[1])
    return new_lst


