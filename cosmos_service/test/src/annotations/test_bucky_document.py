from .annotations_base import BaseAnnotationComparisonTest
import json

PDF_NAME='bucky'

class TestBuckyDocumentAnnotations(BaseAnnotationComparisonTest):

    def setup_method(self):
        self._setup(PDF_NAME)

    def test_compare_figure_counts(self):
        self.check_document_count("Figure")

    def test_compare_overlapping_figure_areas(self):
        self.check_document_overlap("Figure")

    def test_compare_figure_caption_counts(self):
        self.check_document_count("Figure Caption")

    def test_compare_overlapping_figure_caption_areas(self):
        self.check_document_overlap("Figure Caption")

    def test_compare_table_counts(self):
        self.check_document_count("Table")

    def test_compare_overlapping_table_areas(self):
        self.check_document_overlap("Table")

    def test_compare_table_caption_counts(self):
        self.check_document_count("Table Caption")

    def test_compare_overlapping_table_caption_areas(self):
        self.check_document_overlap("Table Caption")

    def test_compare_equation_counts(self):
        self.check_document_count("Equation")

    def test_compare_overlapping_equation_areas(self):
        self.check_document_overlap("Equation")
