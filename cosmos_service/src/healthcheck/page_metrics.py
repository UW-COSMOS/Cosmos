""" Classes for aggregating per-page and per-document comparisons between COSMOS output and a known baseline """

from pydantic import BaseModel, Field, computed_field
from typing import List
from numpy import arange

AP_THRESH = arange(0.5,1,0.05) # average of all precisions from 0.5 to 0.95 in 0.05 increments
AP50_THRESH = [0.5]
AP75_THRESH = [0.75]

class AnnotationBounds(BaseModel):
    page_num: int = Field(description="Page Number")
    postprocess_cls: str = Field(description="The label assigned to the region by Cosmos")
    postprocess_score: float = 0 # for internal use only
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
    average_precision: float = Field(description="Average precision of the COSMOS-identified regions on the page")
    ap50: float = Field(description="Precision of the COSMOS-identified regions on the page with a 0.5 iou threshold")
    ap75: float = Field(description="Precision of the COSMOS-identified regions on the page with a 0.75 iou threshold")
    ious: List[float] = Field(exclude=True) # for internal use only
    postprocess_scores: List[float] = Field(exclude=True) # for internal use only

    @computed_field(description="Whether the correct count of regions was identified by COSMOS")
    @property
    def count_in_bounds(self) -> bool:
        return self.expected_count == self.cosmos_count

    @staticmethod
    def _average_precision_score(true_labels, scores):
        """ Approximate the area under the (precision, recall) curve for the given set of labels, 
        ordered by confidence score. 
        https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)#Average_precision
        """
        
        # sort the true labels according to their confidence score, descending
        sorted_true_labels = [label for score, label in sorted(zip(scores, true_labels), reverse=True)]
        total_true_count = sum(sorted_true_labels)

        if total_true_count == 0:
            return 0

        # Calculate (recall, precision) coordinates at each step of the array
        rc_pairs = []
        for i,_ in enumerate(sorted_true_labels):
            correct_sum = sum(sorted_true_labels[:i+1])
            recall = correct_sum / total_true_count
            precision = correct_sum / (i + 1)
            rc_pairs.append((recall, precision))
        
        # Approximate the area under the curve as the sum of precision * delta_recall
        precision_sum = 0
        previous_recall = 0
        for recall, precision in rc_pairs:
            delta_recall = recall - previous_recall
            precision_sum += precision * delta_recall
            previous_recall = recall
        
        return precision_sum
            

    @staticmethod
    def _precision_at_thresholds(ious: List[float], scores: List[float], thresholds: List[float]):
        if len(ious) == 0:
            return 0
        all_precisions = []
        for thresh in thresholds:
            true_labels = [i >= thresh for i in ious]
            all_precisions.append(PageAnnotationComparison._average_precision_score(true_labels, scores))

        return sum(all_precisions) / len(all_precisions)

    @staticmethod
    def _average_precisions(expected_bounds: List[AnnotationBounds], actual_bounds: List[AnnotationBounds]):
        # Sort bounds in order of confidence

        if len(actual_bounds) == 0 or len(expected_bounds) == 0:
            return {'ious': [], 'postprocess_scores': [], 'average_precision': 0, 'ap50': 0, 'ap75': 0}
        # For each expected bound, find the best i-o-u ratio from among the cosmos bounds on the same page
        ious = [max(e.intersection_over_union(a) for e in expected_bounds) for a in actual_bounds]
        postprocess_scores = [a.postprocess_score for a in actual_bounds]
        # return the average of all the best i-o-us on the page
        return {
            'ious': ious,
            'postprocess_scores': postprocess_scores,
            'average_precision': PageAnnotationComparison._precision_at_thresholds(ious, postprocess_scores, AP_THRESH),
            'ap50': PageAnnotationComparison._precision_at_thresholds(ious, postprocess_scores, AP50_THRESH),
            'ap75': PageAnnotationComparison._precision_at_thresholds(ious, postprocess_scores, AP75_THRESH),
        }

    @staticmethod
    def from_bounds(page:int, expected_bounds: List[AnnotationBounds], actual_bounds: List[AnnotationBounds]):
        return PageAnnotationComparison(
            page=page,
            expected_count= len(expected_bounds), 
            cosmos_count=len(actual_bounds),
            expected_area=sum(e.area() for e in expected_bounds),
            cosmos_area=sum(a.area() for a in actual_bounds),
            **PageAnnotationComparison._average_precisions(expected_bounds, actual_bounds)
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

    @computed_field(description="Whether the correct count of regions was identified by COSMOS")
    @property
    def count_in_bounds(self) -> bool:
        return self.document_expected_count == self.document_cosmos_count

    @computed_field(description="The average precision of every COSMOS-identified region in the document")
    @property
    def average_precision(self) -> float:
        all_ious = sum([p.ious for p in self.page_comparisons], [])
        all_postprocess_scores = sum([p.postprocess_scores for p in self.page_comparisons], [])
        return PageAnnotationComparison._precision_at_thresholds(all_ious, all_postprocess_scores, AP_THRESH)

    @computed_field(description="The precision of every COSMOS-identified region in the document with a 0.5 iou threshold")
    @property
    def ap50(self) -> float:
        all_ious = sum([p.ious for p in self.page_comparisons], [])
        all_postprocess_scores = sum([p.postprocess_scores for p in self.page_comparisons], [])
        return PageAnnotationComparison._precision_at_thresholds(all_ious, all_postprocess_scores, AP50_THRESH)

    @computed_field(description="The precision of every COSMOS-identified region in the document with a 0.75 iou threshold")
    @property
    def ap75(self) -> float:
        all_ious = sum([p.ious for p in self.page_comparisons], [])
        all_postprocess_scores = sum([p.postprocess_scores for p in self.page_comparisons], [])
        return PageAnnotationComparison._precision_at_thresholds(all_ious, all_postprocess_scores, AP75_THRESH)
