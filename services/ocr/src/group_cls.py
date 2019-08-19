"""
Group classes
"""

def calculate_iou(box1, box2, contains=False):
    # Shamelessly adapted from
    # https://stackoverflow.com/questions/25349178/calculating-percentage-of-bounding-box-overlap-for-image-detector-evaluation
    # determine the coordinates of the intersection rectangle
    """
    Calculate the IoU of two boxes
    :param box1: (tl_x, tl_y, br_x, br_y) box 1
    :param box2: (tl_x, tl_y, br_x, br_y) box 1
    :return: IoU overlap
    """
    x_left = max(box1[0], box2[0])
    y_top = max(box1[1], box2[1])
    x_right = min(box1[2], box2[2])
    y_bottom = min(box1[3], box2[3])
    
    if contains:
        if box2[0] <= box1[0] and box2[1] <= box1[1] and box2[2] >= box1[2] and box2[3] >= box1[3]:
            return 1.0

    if x_right < x_left or y_bottom < y_top:
        return 0.0

    # The intersection of two axis-aligned bounding boxes is always an
    # axis-aligned bounding box
    intersection_area = (x_right - x_left) * (y_bottom - y_top)

    # compute the area of both AABBs
    bb1_area = (box1[2] - box1[0]) * (box1[3] - box1[1])
    bb2_area = (box2[2] - box2[0]) * (box2[3] - box2[1])

    # compute the intersection over union by taking the intersection
    # area and dividing it by the sum of prediction + ground-truth
    # areas - the interesection area
    iou = intersection_area / float(bb1_area + bb2_area - intersection_area)
    return iou


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


