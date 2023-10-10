""" Classes for aggregating per-page an per-document comparisons """

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

    def overlap(self, other: 'AnnotationBounds'):
        """ Get the intersecting area of two rectangles """
        # via https://stackoverflow.com/a/27162334

        (my_x0, my_y0, my_x1, my_y1) = self.bounding_box
        (their_x0, their_y0, their_x1, their_y1) = other.bounding_box

        dx = min(my_x1, their_x1) - max(my_x0, their_x0)
        dy = min(my_y1, their_y1) - max(my_y0, their_y0)

        return dx * dy if dx > 0 and dy > 0 else 0

class PageAnnotationComparison(BaseModel):

    page: int = Field(description="Page Number")
    expected_count: int = Field(description="Expected count of regions with the given label on the page")
    cosmos_count: int = Field(description="Count of regions with the given label identified by COSMOS on the page")

    expected_area: int = Field(description="Expected area of regions with the given label on the page")
    cosmos_area: int = Field(description="Area of regions with the given label identified by COSMOS on the page")
    overlapping_area: int = Field(description="Overlapping area between expected regions and COSMOS-identified regions")

    @computed_field(description="Percentage of the expected area identified by COSMOS")
    @property
    def overlap_percent(self) -> float:
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

    @computed_field(description="Whether the correct count of regions was identified by COSMOS")
    @property
    def count_in_bounds(self) -> bool:
        return self.expected_count == self.cosmos_count

    @computed_field(description=f"Whether over 90% of the expected area was identified by COSMOS")
    @property
    def overlap_in_bounds(self) -> bool:
        return self.overlap_percent >= 0.9 and self.overlap_percent <= 1.1

    @staticmethod
    def from_bounds(page:int, expected_bounds: List[AnnotationBounds], actual_bounds: List[AnnotationBounds]):
        return PageAnnotationComparison(
            page=page,
            expected_count= len(expected_bounds), 
            cosmos_count=len(actual_bounds),
            expected_area=sum(e.area() for e in expected_bounds),
            cosmos_area=sum(a.area() for a in actual_bounds),
            overlapping_area = sum(e.overlap(a) for e in expected_bounds for a in actual_bounds)
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

    @computed_field(description="The percentage of the expected area of regions with the given label identified by COSMOS")
    @property
    def document_overlap_percent(self) -> float:
        cosmos_area_per_page = [p.cosmos_area for p in self.page_comparisons]
        expected_area_per_page = [p.expected_area for p in self.page_comparisons]
        overlap_area_per_page = [p.overlapping_area for p in self.page_comparisons]

        if sum(cosmos_area_per_page) == 0 and sum(expected_area_per_page) == 0:
            return 1
        elif sum(expected_area_per_page) == 0 and sum(cosmos_area_per_page) > 0:
            return float('inf')
        else:
            return sum(overlap_area_per_page) / sum(expected_area_per_page)

    @computed_field(description="Whether the correct count of regions was identified by COSMOS")
    @property
    def count_in_bounds(self) -> bool:
        return self.document_expected_count == self.document_cosmos_count

    @computed_field(description=f"Whether over 90% of the expected area was identified by COSMOS")
    @property
    def overlap_in_bounds(self) -> bool:
        return self.document_overlap_percent >= 0.9 and self.document_overlap_percent <= 1.1
