""" Classes for aggregating per-page an per-document comparisons """

from dataclasses import dataclass
from ....src.healthcheck.page_metrics import PageAnnotationComparison

@dataclass
class PageExpectedValuePrinter:
    """ Utility class for displaying the page on which some metric falls outside of the expected range """

    comparison: PageAnnotationComparison

    def __init__(self, comparison: PageAnnotationComparison):
        self.comparison = comparison

    def print_count_mismatch(self) -> str:
        return f"page {self.comparison.page}: expected={self.comparison.expected_count} actual={self.comparison.cosmos_count}"

    def print_overlap_mismatch(self) -> str:
        return f"page {self.comparison.page}: expected=(0.9, 1.1) actual={self.comparison.overlap_percent}"

class DocumentExpectedCountPrinter:
    """ Utility class for displaying all pages in a document for which the annotation count falls outside of the expected range """
    page_values: list[PageExpectedValuePrinter]
    comparison_type: str

    def __init__(self, comparison_type, page_values: list[PageAnnotationComparison]):
        self.page_values = [PageExpectedValuePrinter(p) for p in page_values if not p.count_in_bounds]
        self.comparison_type = comparison_type

    def ok(self):
        return len(self.page_values) == 0
    
    def error_message(self):
        return f"Incorrect {self.comparison_type} on {len(self.page_values)} page(s)\n" + "\n".join([m.print_count_mismatch() for m in self.page_values])

class DocumentExpectedOverlapPrinter:
    """ Utility class for displaying all pages in a document for which the annotation count falls outside of the expected range """
    page_values: list[PageExpectedValuePrinter]
    comparison_type: str

    def __init__(self, comparison_type, page_values: list[PageAnnotationComparison]):
        self.page_values = [PageExpectedValuePrinter(p) for p in page_values if not p.overlap_in_bounds]
        self.comparison_type = comparison_type

    def ok(self):
        return len(self.page_values) == 0
    
    def error_message(self):
        return f"Incorrect {self.comparison_type} on {len(self.page_values)} page(s)\n" + "\n".join([m.print_overlap_mismatch() for m in self.page_values])
