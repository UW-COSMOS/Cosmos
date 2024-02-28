"""
Helper functions that try to extract the individual equations
out of an equation-labeled image segment
"""
from typing import List, Tuple, Dict
from collections import namedtuple
from ..proposals.connected_components import get_proposals
from PIL import Image
import fitz
import re

COSMOS_DOC_HEIGHT=1920
LABEL_PATTERN=re.compile(r'[\(ð][1-9A-Z]+.?[0-9A-Za-z]*[\)Þ]',re.MULTILINE)
HI_RES_DPI=300

LABEL_OVERLAP = 0.9 # Remove any segments that overlap at least this % with a label bb
PADDING = 5 # Pad the very narrowly cropped equation segments back out a bit

# Convenience class to assign property names to the 4 tuple of ints used for segment coordinates
Bounds = namedtuple("Bounds", ("left", "top", "right", "bottom"))

def _get_label_bbs(pymu_page: fitz.Page, labels: List[str], reference_frame: Bounds) -> List[Bounds]:
    """ Given a list of label texts on a page, find the bounding boxes
    for those label texts. Then, convert from PyMuPDF coordinates back to COSMOS coordinates
    """
    # TODO clean up frame of reference shifting
    x0, y0, *_ = reference_frame
    height_ratio = (pymu_page.rect[3] - pymu_page.rect[1]) / COSMOS_DOC_HEIGHT
    label_bbs = []
    for label in labels:
        label_bbs.extend(pymu_page.search_for(label))
    unscaled_bbs = [b/height_ratio for b in label_bbs]
    return [Bounds(b.x0 - x0, b.y0 - y0, b.x1 - x0, b.y1 - y0) for b in unscaled_bbs]

def _get_scaled_rect(pymu_page: fitz.Page, bounds: Bounds) -> Tuple[fitz.Page, fitz.Rect]:
    """ Given a bounding box in the COSMOS coordinate system and a PyMuPDF page,
    return that bounding box in the PyMuPDF coordinate system """
    height_ratio = (pymu_page.rect[3] - pymu_page.rect[1]) / COSMOS_DOC_HEIGHT
    return fitz.Rect(*bounds) * height_ratio

# TODO don't manually reimplement basic geometry functions

def _middle(bounds: Bounds):
    """Get the vertical center of a Bounds"""
    return (bounds.top + bounds.bottom) / 2

def _overlap(r1: Bounds, r2: Bounds):
    """ Get the overlapping area between two Bounds"""
    return max(0, min(r1.right, r2.right) - max(r1.left, r2.left)) * \
        max(0, min(r1.bottom, r2.bottom) - max(r1.top, r2.top))

def _area(r1: Bounds):
    """ Get the area of the Bounds """
    return (r1.right - r1.left) * (r1.bottom - r1.top)

def _overlaps_with_label(bounds: Bounds, label_bounds: List[Bounds]):
    """ Return whether a Bounds is largely overlapping with a label """
    area = _area(bounds)
    return any(_overlap(bounds, lb) > 0.9 * area for lb in label_bounds)

def find_labels_for_equation(pymu_page: fitz.Page, bounds: Bounds):
    """ Find the text in the text layer matching `(X.Y)` that is in close proximity 
    to the equation bounding box """
    rect = _get_scaled_rect(pymu_page, bounds)
    # guess which "half" of the page the equation is on, then look for a label
    # across the whole column
    col_width = pymu_page.rect[2] / 2
    if rect.x1 < col_width:
        extended_bounds = fitz.Rect(0, rect.y0, col_width, rect.y1)
    elif rect.x0 > col_width:
        extended_bounds = fitz.Rect(col_width, rect.y0, 2 * col_width, rect.y1)
    else:
        extended_bounds = fitz.Rect(0, rect.y0, 2 * col_width, rect.y1)
    eqn_text = pymu_page.get_textbox(extended_bounds)
    return re.findall(LABEL_PATTERN, eqn_text)

def _pad(bound: Bounds, padding: int = PADDING):
    """Pad a Bounds by expanding it slightly on all sides"""
    return Bounds(
        bound.left - padding,
        bound.top - padding,
        bound.right + padding,
        bound.bottom + padding 
    )

def _group_equations_by_nearest_label(segments: List[Bounds], label_segments: List[Bounds], reference_frame: Bounds):
    """
    Re-group an equation image that was partitioned along a very narrow whitespace margin.
    Identify the labels in the equation system based on their right-justification,
    then group all other segments of the equation by their nearest label-identified segment
    """
    # TODO clean up frame of reference shifting
    x0, y0, *_ = reference_frame
    named_segments = [Bounds(*s) for s in segments]
    non_label_segments = [s for s in named_segments if not _overlaps_with_label(s, label_segments)]

    # backup in case there are no labels in the image
    if len(label_segments) == 0:
        label_segments.append(Bounds(0, 0, 0, 0))

    label_groups : Dict[Bounds, List[Bounds]] = {s: [] for s in label_segments}

    for segment in non_label_segments:
        # find the nearest vertical label
        nearest_label = sorted(label_segments, key = lambda ls: abs(_middle(ls) - _middle(segment)))[0]
        label_groups[nearest_label].append(segment)

    grouped_bounds = [
        Bounds(
            min([s.left for s in sg]) + x0,
            min([s.top for s in sg]) + y0,
            max([s.right for s in sg]) + x0,
            max([s.bottom for s in sg]) + y0
        )
        for sg in label_groups.values() if len(sg)
    ]

    padded_bounds = [_pad(b) for b in grouped_bounds]

    return padded_bounds

def split_equation_system(pymu_page: fitz.Page, target) -> Tuple[Image.Image, Bounds, List[Bounds]]:
    """
    Propose sub-images for an equation-labeled image, which frequently contain multiple equations
    """
    # Frame of reference for eqn splitting: the bounding box of the original system of equations
    reference_bb = _pad(Bounds(*target['bounding_box']))
    img_base = Image.open(target['img_pth']).convert('RGB').crop(reference_bb)

    label_texts = find_labels_for_equation(pymu_page, reference_bb)
    label_bbs = _get_label_bbs(pymu_page, label_texts, reference_bb)
    # Systems of equations are frequently closely grouped, need to find regions again with a
    # smaller whitespace margin
    all_sub_regions = get_proposals(img_base, blank_row_height=3, max_obj_count=50)
    return _group_equations_by_nearest_label(all_sub_regions, label_bbs, reference_bb)


def save_high_res_img(pymu_page: fitz.Page, bounds: Bounds, path, dpi=HI_RES_DPI):
    """ Save an image with the given dpi using PyMuPDF. """
    pymu_page.get_pixmap(dpi=dpi, clip=_get_scaled_rect(pymu_page, bounds)).save(path)
