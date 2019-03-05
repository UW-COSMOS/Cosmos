import torch
X = 0
Y = 1
H = 2
W = 3
X2 = 2
Y2 = 3





def cross_boundary(bboxes, img_size, device, remove=True):
    """
    get the indexes of boxes which cross the image boundary
    :param bboxes: [L x 4]
    :param img_size: W the side length of the square image
    :param device: the device to do computation on
    :return: [idx_1, idx_2] of indexes in the L dimension
    """
    # turn w,h coordinates into X2, Y2 coordinates
    bbox_coords = bboxes.get("xyxy")
    if remove:
        conforming = ((bbox_coords <= img_size) * (bbox_coords >= 0)).sum(dim=1)
        mask = conforming == 4
        return bboxes[mask, :]
    else:
        # clamp coords to the desired region
        bbox_coords = bbox_coords.clamp(0, img_size)
        return bbox_coords.get("xyhw")


