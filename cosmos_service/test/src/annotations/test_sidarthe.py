from .annotations_base import BaseAnnotationComparisonTest, equals_comparator, in_bounds_comparator
import json

PDF_NAME='sidarthe'
REMOTE_URL='https://www.nature.com/articles/s41591-020-0883-7.pdf'
AREA_BOUNDS=(0.9,1.1)

class TestSidartheAnnotations(BaseAnnotationComparisonTest):

    def setup_method(self):
        self._setup(PDF_NAME, REMOTE_URL)

    def test_compare_figure_counts(self):
        fc = self.compare_pages_for_label("Figure")
        failures = fc.get_failures_per_page(
            fc.expected_counts, fc.cosmos_counts, equals_comparator)
        assert failures.ok(), failures.error_message()

    def test_compare_overlapping_areas(self):
        fc = self.compare_pages_for_label("Figure")
        failures = fc.get_failures_per_page(
            [AREA_BOUNDS for _ in fc.expected_counts],
            fc.overlap_percents, 
            in_bounds_comparator)
        assert failures.ok(), failures.error_message()

    def test_compare_total_areas(self):
        figure_comparisons = self.compare_pages_for_label("Figure")
        failures = figure_comparisons.get_failures_per_page(
            [AREA_BOUNDS for _ in figure_comparisons.expected_counts],
            figure_comparisons.area_ratios, 
            in_bounds_comparator)
        assert failures.ok(), failures.error_message()
