from .annotations_base import BaseAnnotationComparisonTest, equals_comparator, in_bounds_comparator
import json

PDF_NAME='sidarthe'
REMOTE_URL='https://www.nature.com/articles/s41591-020-0883-7.pdf'

class TestSidartheAnnotations(BaseAnnotationComparisonTest):

    def setup_method(self):
        self._setup(PDF_NAME, REMOTE_URL)

    def test_compare_figure_counts(self):
        self.check_count_per_page("Figure")

    def test_compare_overlapping_figure_areas(self):
        self.check_overlap_per_page("Figure")

    def test_compare_figure_caption_counts(self):
        self.check_count_per_page("Figure Caption")

    def test_compare_overlapping_figure_caption_areas(self):
        self.check_overlap_per_page("Figure Caption")

    def test_compare_table_counts(self):
        self.check_count_per_page("Table")

    def test_compare_overlapping_table_areas(self):
        self.check_overlap_per_page("Table")

    def test_compare_table_caption_counts(self):
        self.check_count_per_page("Table Caption")

    def test_compare_overlapping_table_caption_areas(self):
        self.check_overlap_per_page("Table Caption")

    def test_compare_equation_counts(self):
        self.check_count_per_page("Equation")

    def test_compare_overlapping_equation_areas(self):
        self.check_overlap_per_page("Equation")
