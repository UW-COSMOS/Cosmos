""" Classes for aggregating per-page and per-document comparisons between COSMOS output and a known baseline """

from pydantic import BaseModel, Field, computed_field
from typing import List

class AnnotationBounds(BaseModel):
    page_num: int = Field(description="Page Number")
    postprocess_cls: str = Field(description="The label assigned to the region by Cosmos")
    bounding_box: List[int] = Field(description="Region bounds (x0, y0, x1, y1)")

    def area(self):
        """ Get the area of a bounding box"""
        (x0, y0, x1, y1) = self.bounding_box
        return (x1 - x0) * (y1 - y0)

    def intersection(self, other: 'AnnotationBounds'):
        """ Get the intersecting area of two rectangles """
        # via https://stackoverflow.com/a/27162334

        (my_x0, my_y0, my_x1, my_y1) = self.bounding_box
        (their_x0, their_y0, their_x1, their_y1) = other.bounding_box

        dx = min(my_x1, their_x1) - max(my_x0, their_x0)
        dy = min(my_y1, their_y1) - max(my_y0, their_y0)

        return dx * dy if dx > 0 and dy > 0 else 0


    def union(self, other: 'AnnotationBounds'):
        """ Get the union area of two rectangles """
        return self.area() + other.area() - self.intersection(other)


    def intersection_over_union(self, other: 'AnnotationBounds'):
        """ Get the intersection-over-union of two rectangles """
        return self.intersection(other) / self.union(other)

class PageAnnotationComparison(BaseModel):

    page: int = Field(description="Page Number")
    expected_count: int = Field(description="Expected count of regions with the given label on the page")
    cosmos_count: int = Field(description="Count of regions with the given label identified by COSMOS on the page")

    expected_area: int = Field(description="Expected area of regions with the given label on the page")
    cosmos_area: int = Field(description="Area of regions with the given label identified by COSMOS on the page")
    average_iou: float = Field(description="Average intersection-over-union for all expected regions on the page")
    max_ious: List[float] = Field(exclude=True) # for internal use only

    @computed_field(description="Whether the correct count of regions was identified by COSMOS")
    @property
    def count_in_bounds(self) -> bool:
        return self.expected_count == self.cosmos_count

    @computed_field(description=f"Whether over 90% of the expected area was identified by COSMOS")
    @property
    def iou_in_bounds(self) -> bool:
        return self.average_iou >= 0.9


    @staticmethod
    def _average_iou(expected_bounds: List[AnnotationBounds], actual_bounds: List[AnnotationBounds]):
        if len(expected_bounds) == 0 and len(actual_bounds) == 0:
            return {'max_ious': [], 'average_iou': 1}
        elif len(expected_bounds) == 0 or len(actual_bounds) == 0:
            return {'max_ious': [0 for _ in expected_bounds], 'average_iou': 0}
        # For each expected bound, find the best i-o-u ratio from among the cosmos bounds on the same page
        max_ious = [max(e.intersection_over_union(a) for a in actual_bounds) for e in expected_bounds]
        # return the average of all the best i-o-us on the page
        return {
            'max_ious': max_ious,
            'average_iou': sum(max_ious) / len(expected_bounds)
        }

    @staticmethod
    def from_bounds(page:int, expected_bounds: List[AnnotationBounds], actual_bounds: List[AnnotationBounds]):
        return PageAnnotationComparison(
            page=page,
            expected_count= len(expected_bounds), 
            cosmos_count=len(actual_bounds),
            expected_area=sum(e.area() for e in expected_bounds),
            cosmos_area=sum(a.area() for a in actual_bounds),
            **PageAnnotationComparison._average_iou(expected_bounds, actual_bounds)
        )

class DocumentAnnotationComparison(BaseModel):

    label_class: str
    page_comparisons: List[PageAnnotationComparison]

    @computed_field(description="The expected count of regions with the given label in the whole document")
    @property
    def document_expected_count(self) -> int:
        return sum([p.expected_count for p in self.page_comparisons])

    @computed_field(description="The count of regions with the given label identified by COSMOS in the whole document")
    @property
    def document_cosmos_count(self) -> int:
        return sum([p.cosmos_count for p in self.page_comparisons])

    @computed_field(description="The average of every intersection-over-union for the expected regions in the document")
    @property
    def document_average_iou(self) -> float:
        all_ious = sum([p.max_ious for p in self.page_comparisons], [])
        return sum(all_ious) / len(all_ious)

    @computed_field(description="Whether the correct count of regions was identified by COSMOS")
    @property
    def count_in_bounds(self) -> bool:
        return self.document_expected_count == self.document_cosmos_count

    @computed_field(description=f"Whether over 90% of the expected area was identified by COSMOS")
    @property
    def iou_in_bounds(self) -> bool:
        return self.document_average_iou >= 0.9
