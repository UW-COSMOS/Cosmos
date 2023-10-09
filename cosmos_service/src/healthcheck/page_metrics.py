""" Classes for aggregating per-page an per-document comparisons """

from dataclasses import dataclass
from model import AnnotationBounds

def equals_comparator(expected, actual):
    return expected == actual

def in_bounds_comparator(expected, actual):
    return actual >= expected[0] and actual <= expected[1]

Rectangle = tuple[int,int,int,int]

def rectangle_intersection(r1: Rectangle, r2: Rectangle):
    """ Get the intersecting area of two rectangles """
    # via https://stackoverflow.com/a/27162334
    dx = min(r1[2], r2[2]) - max(r1[0],r2[0])
    dy = min(r1[3], r2[3]) - max(r1[1],r2[1])

    return dx * dy if dx > 0 and dy > 0 else 0

def rectangle_area(r1: Rectangle):
    """ Get the area of a rectangle """
    length = r1[2] - r1[0]
    width = r1[3] - r1[1]
    return length * width

@dataclass
class PageAnnotationComparison:

    page: int
    expected_count: int
    cosmos_count: int

    expected_area: int
    cosmos_area: int
    overlapping_area: int


    @property
    def area_ratio(self):
        """ The total area marked by cosmos compared to the total expected area"""
        # avoid dividing by zero
        if self.expected_area == 0 and self.cosmos_area == 0:
            return 1
        elif self.expected_area == 0 and self.cosmos_area > 0:
            return float('inf')
        else:
            return self.cosmos_area / float(self.expected_area)

    @property
    def overlap_percent(self):
        """ The regions marked by cosmos that coincide with the expected regions, 
        compared to the total expected area 
        """
        # avoid dividing by zero
        if self.expected_area == 0 and self.cosmos_area == 0:
            return 1
        elif self.expected_area == 0 and self.cosmos_area > 0:
            return float('inf')
        else:
            return self.overlapping_area / float(self.expected_area)


    @staticmethod
    def from_bounds(page:int, expected_bounds: list[AnnotationBounds], actual_bounds: list[AnnotationBounds]):
        return PageAnnotationComparison(
            page=page,
            expected_count= len(expected_bounds), 
            cosmos_count=len(actual_bounds),
            expected_area=sum(e.area() for e in expected_bounds),
            cosmos_area=sum(a.area() for a in actual_bounds),
            overlapping_area = sum(e.overlap(a) for e in expected_bounds for a in actual_bounds)
        )

@dataclass
class PageExpectedValue:
    """ Utility class for recording the page on which some metric falls outside of the expected range """
    page: int
    expected: float
    actual: float

class DocumentAnnotationComparison:

    label_class: str
    page_comparisons: list[PageAnnotationComparison]
    failed_pages: lst

    def __init__(self, page_comparisons, label_class):
        self.label_class = label_class
        self.page_comparisons = page_comparisons

    @property
    def expected_counts(self):
        return [p.expected_count for p in self.page_comparisons]

    @property
    def cosmos_counts(self):
        return [p.cosmos_count for p in self.page_comparisons]

    @property
    def expected_area_per_page(self):
        return [p.expected_area for p in self.page_comparisons]

    @property
    def cosmos_area_per_page(self):
        return [p.cosmos_area for p in self.page_comparisons]

    @property
    def overlap_area_per_page(self):
        return [p.overlapping_area for p in self.page_comparisons]

    @property
    def area_ratios(self):
        return [p.area_ratio for p in self.page_comparisons]

    @property
    def overlap_percents(self):
        return [p.overlap_percent for p in self.page_comparisons]

    def get_failures_per_page(self, comparison_metric, expected, actual, meets_cond):
        failures = [PageExpectedValue(i+1,e,a) for i, (e,a) in enumerate(zip(expected, actual)) if not meets_cond(e,a)]
        return DocumentExpectedValues(failures, comparison_metric)

    @property
    def document_expected_count(self):
        return sum(self.expected_counts)

    @property
    def document_cosmos_count(self):
        return sum(self.cosmos_counts)

    @property
    def document_overlap_percent(self):

        if sum(self.cosmos_area_per_page) == 0 and sum(self.expected_area_per_page) == 0:
            return 1
        elif sum(self.expected_area_per_page) == 0 and sum(self.cosmos_area_per_page) > 0:
            return float('inf')
        else:
            return sum(self.overlap_area_per_page) / sum(self.expected_area_per_page)
