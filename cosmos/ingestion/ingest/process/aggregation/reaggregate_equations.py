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

COSMOS_DOC_SIZE=1920
LABEL_PATTERN=re.compile(r'[\(ð][1-9A-Z]+.?[0-9A-Z]*[\)Þ]',re.MULTILINE)
HI_RES_DPI=300

RIGHT_JUSTIFY_EDGE = 15 # Farthest from an image's right edge that a label can be
LABEL_MAX_WIDTH = 60 # Max width of a label
LABEL_MAX_HEIGHT = 40 # Max height of a label
LABEL_OVERLAP = 0.9 # Remove any segments that overlap at least this % with a label bb
PADDING = 5 # Pad the very narrowly cropped equation segments back out a bit

# Convenience class to assign property names to the 4 tuple of ints used for segment coordinates
Bounds = namedtuple("Bounds", ("left", "top", "right", "bottom"))

def _find_eqn_labels(text):
    return re.findall(LABEL_PATTERN, text)

def _get_label_bbs(pymu_page: fitz.Page, labels: list[str], x0, y0) -> list[Bounds]:
    height_ratio = (pymu_page.rect[3] - pymu_page.rect[1]) / COSMOS_DOC_SIZE
    label_bbs = []
    for label in labels:
        label_bbs.extend(pymu_page.search_for(label))
    unscaled_bbs = [b/height_ratio for b in label_bbs]
    return [Bounds(b.x0 - x0, b.y0 - y0, b.x1 - x0, b.y1 - y0) for b in unscaled_bbs]

def _get_page_and_scaled_rect(source_pdf, page, bounds: Bounds) -> Tuple[fitz.Page, fitz.Rect]:
    pymu_doc = fitz.Document(source_pdf)
    pymu_page = pymu_doc[page]
    # Cosmos documents are scaled to a uniform height,
    # need to scale back to original reference frame
    height_ratio = (pymu_page.rect[3] - pymu_page.rect[1]) / COSMOS_DOC_SIZE

    return pymu_page, fitz.Rect(*bounds) * height_ratio

# TODO don't manually reimplement basic geometry functions

def _overlap(r1: Bounds, r2: Bounds):
    return max(0, min(r1.right, r2.right) - max(r1.left, r2.left)) * \
        max(0, min(r1.bottom, r2.bottom) - max(r1.top, r2.top))

def _area(r1: Bounds):
    return (r1.right - r1.left) * (r1.bottom - r1.top)

def _overlaps_with_label(bounds: Bounds, label_bounds: list[Bounds]):
    area = _area(bounds)
    return any(_overlap(bounds, lb) > 0.9 * area for lb in label_bounds)

def find_labels_for_equation(source_pdf, page, bounds: Bounds):
    pymu_page, rect = _get_page_and_scaled_rect(source_pdf, page, bounds)
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
    return _find_eqn_labels(eqn_text)

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


def _group_equations_by_nearest_label(segments: List[Bounds], label_segments: List[Bounds]):
    """
    Re-group an equation image that was partitioned along a very narrow whitespace margin.
    Identify the labels in the equation system based on their right-justification,
    then group all other segments of the equation by their nearest label-identified segment
    """

    named_segments = [Bounds(*s) for s in segments]
    non_label_segments = [s for s in named_segments if not _overlaps_with_label(s, label_segments)]

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
        for sg in label_groups.values() if len(sg)
    ]

    padded_bounds = [pad(b) for b in grouped_bounds]

    return padded_bounds

def split_equation_system(source_pdf, target) -> Tuple[Image.Image, Bounds, List[Bounds]]:
    """
    Propose sub-images for an equation-labeled image, which frequently contain multiple equations
    """
    page = target['page_num'] - 1
    padded_bb = pad(Bounds(*target['bounding_box']))
    img_base = Image.open(target['img_pth']).convert('RGB').crop(padded_bb)

    pymu_page, _ = _get_page_and_scaled_rect(source_pdf, page, padded_bb)
    label_texts = find_labels_for_equation(source_pdf, page, padded_bb)
    label_bbs = _get_label_bbs(pymu_page, label_texts, padded_bb.left, padded_bb.top)
    # Systems of equations are frequently closely grouped, need to find regions again with a
    # smaller whitespace margin
    all_sub_regions = get_proposals(img_base, blank_row_height=3, max_obj_count=50)
    return img_base, padded_bb, _group_equations_by_nearest_label(all_sub_regions, label_bbs)


def save_high_res_img(source_pdf, page, bounds: Bounds, path):
    pymu_page, rect = _get_page_and_scaled_rect(source_pdf, page, bounds)
    pymu_page.get_pixmap(dpi=HI_RES_DPI, clip=rect).save(path)
