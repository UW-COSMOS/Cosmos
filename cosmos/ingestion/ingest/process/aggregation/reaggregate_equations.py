"""
Helper functions that try to extract the individual equations
out of an equation-labeled image segment
"""
from typing import List, Tuple, Dict
from collections import namedtuple
from ..proposals.connected_components import get_proposals
from PIL import Image


RIGHT_JUSTIFY_EDGE = 15 # Farthest from an image's right edge that a label can be
LABEL_MAX_WIDTH = 60 # Max width of a label
LABEL_MAX_HEIGHT = 40 # Max height of a label
PADDING = 5 # Pad the very narrowly cropped equation segments back out a bit

# Convenience class to assign property names to the 4 tuple of ints used for segment coordinates
Bounds = namedtuple("Bounds", ("left", "top", "right", "bottom"))

def middle(bounds: Bounds):
    """Get the vertical center of a Bounds"""
    return (bounds.top + bounds.bottom) / 2

def pad(bound: Bounds, padding: int = PADDING):
    """Pad a Bounds by expanding it slightly on all sides"""
    return Bounds(
        bound.left - padding,
        bound.top - padding,
        bound.right + padding,
        bound.bottom + padding 
    )

def is_label(img_width: int, bounds: Bounds):
    """
    Identify equation image segments that are probably equation number labels based on their
    width, height, and proximity to the right edge of the image
    """
    return (bounds.right >= img_width - RIGHT_JUSTIFY_EDGE and 
            bounds.right - bounds.left <= LABEL_MAX_WIDTH and 
            bounds.bottom - bounds.top <= LABEL_MAX_HEIGHT)


def group_equations_by_nearest_label(img_width: int, segments: List[Bounds]):
    """
    Re-group an equation image that was partitioned along a very narrow whitespace margin.
    Identify the labels in the equation system based on their right-justification,
    then group all other segments of the equation by their nearest label-identified segment
    """

    named_segments = [Bounds(*s) for s in segments]

    label_segments = [s for s in named_segments if is_label(img_width, s)]
    # label images can be excluded from final bounds
    non_label_segments = [s for s in named_segments if not is_label(img_width, s)]

    # backup in case there are no labels in the image
    if len(label_segments) == 0:
        label_segments.append(Bounds(0, 0, 0, 0))

    label_groups : Dict[Bounds, List[Bounds]] = {s: [] for s in label_segments}

    for segment in non_label_segments:
        # find the nearest vertical label
        nearest_label = sorted(label_segments, key = lambda ls: abs(middle(ls) - middle(segment)))[0]
        label_groups[nearest_label].append(segment)

    grouped_bounds = [
        Bounds(
            min([s.left for s in sg]),
            min([s.top for s in sg]),
            max([s.right for s in sg]),
            max([s.bottom for s in sg])
        )
        for sg in label_groups.values()
    ]

    padded_bounds = [pad(b) for b in grouped_bounds]

    return padded_bounds

def split_equation_system(target) -> Tuple[Image.Image, Bounds, List[Bounds]]:
    """
    Propose sub-images for an equation-labeled image, which frequently contain multiple equations
    """
    # TODO make this behavior toggle-able
    padded_bb = pad(Bounds(*target['bounding_box']))
    img_base = Image.open(target['img_pth']).convert('RGB').crop(padded_bb)
    # Systems of equations are frequently closely grouped, need to find regions again with a
    # smaller whitespace margin
    all_sub_regions = get_proposals(img_base, blank_row_height=3, max_obj_count=50)
    return img_base, padded_bb, group_equations_by_nearest_label(img_base.width, all_sub_regions)
