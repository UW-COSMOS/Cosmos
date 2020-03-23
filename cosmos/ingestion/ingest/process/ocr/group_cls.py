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
    for bb, cls_list in obj_list:
        iou = calculate_iou(bb, box)
        if check_cls is not None and iou > 0 and cls_list[0][1] in check_cls:
            continue
        if check_above_below:
            intersection = max(0, min(bb[2], box[2]) - max(bb[0], box[0]))
            if intersection == 0:
                return False
        if iou == 0:
            continue
        return False
    return True

def check_overlap_bb(bb, box):
    iou = calculate_iou(bb, box)
    return iou > 0

def group_cls(obj_list, g_cls, do_table_merge=False, merge_over_classes=None):
    """
    Group bounding boxes of the ground truth class if they do not overlap with other classes
    :param obj_list: [(coords), [(score, cls)]], the list of classes and their ranking for each bounding box
    :param g_cls: the groundtruth class to merge
    :param do_table_merge: flag to do special table case.
    :param merge_over_classes: Optional list of classes to ignore when considering merging
    """
    nbhds = []
    for obj in obj_list:
        coords, cls_list = obj
        if cls_list[0][1] == g_cls:
            if len(nbhds) == 0:
                nbhds.append((coords, cls_list))
                continue
            new_nbhd_list = []
            added = False
            for nbhd_bb in nbhds:
                # construct a bounding box over this table and the neighborhood
                nbhd_bb, cls_list2 = nbhd_bb
                new_box = [min(nbhd_bb[0], coords[0]), min(nbhd_bb[1], coords[1]), max(nbhd_bb[2], coords[2]), max(nbhd_bb[3], coords[3])]
                ccls = [g_cls]
                if merge_over_classes is not None:
                    ccls.extend(merge_over_classes)
                if check_overlap(obj_list, new_box, check_cls=ccls):
                    new_nbhd_list.append((new_box, cls_list2 if cls_list2[0][0] >= cls_list[0][0] and cls_list2[0][1] == g_cls else cls_list))
                    added = True
                else:
                    new_nbhd_list.append((nbhd_bb, cls_list2))
            # If we didn't merge with an existing neighborhood, create a new neighborhood
            if not added:
                new_nbhd_list.append((coords, cls_list))
            nbhds = new_nbhd_list
    # During a table merge, there is a chance that multiple neighborhoods are intersecting. In this case, we merge the intersecting neighborhoods
    if do_table_merge:
        while True:
            new_nbhds = []
            merged_nbhds = []
            # Now we check for intersecting table neighborhoods
            for nbhd in nbhds:
                nbhd, cls_list = nbhd
                for nbhd2 in nbhds:
                    nbhd2, cls_list2 = nbhd2
                    if nbhd2 == nbhd:
                        continue
                    iou = calculate_iou(nbhd, nbhd2)
                    if iou > 0:
                        # merge the neighborhoods
                        new_box = [min(nbhd[0], nbhd2[0]), min(nbhd[1], nbhd2[1]), max(nbhd[2], nbhd2[2]), max(nbhd[3], nbhd2[3])]
                        if new_box in new_nbhds:
                            continue
                        # keep track of the original neighborhoods we merge
                        merged_nbhds.append(nbhd)
                        merged_nbhds.append(nbhd2)
                        new_nbhds.append((new_box, cls_list2 if cls_list2[0][0] >= cls_list[0][0] else cls_list))
            for nbhd in nbhds:
                nbhd, cls_list = nbhd
                if nbhd not in merged_nbhds:
                    new_nbhds.append((nbhd, cls_list))
            # continue until convergence
            if new_nbhds == nbhds:
                break
            nbhds = new_nbhds

    filter_objs = []
    # now we need to check for other overlaps
    for nbhd in nbhds:
        nbhd, cls_list = nbhd
        for obj in obj_list:
            coords, cls_list = obj
            obj_iou = calculate_iou(coords, nbhd)
            if obj_iou > 0:
                filter_objs.append(obj)
    # Filter the objects we merged over
    obj_list = [o for o in obj_list if o not in filter_objs]
    new_obj_list = []
    for obj in obj_list:
        coords, cls_list = obj
        if cls_list[0][1] != g_cls:
            new_obj_list.append(obj)
    for nbhd in nbhds:
        nbhd, cls_list = nbhd
        assert cls_list[0][1] == g_cls
        new_obj_list.append((nbhd, cls_list))
    return new_obj_list


