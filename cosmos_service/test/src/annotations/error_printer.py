""" 
Utility classes for printing detailed information about discrepancies between 
COSMOS and manual document annotations in pytest error output
"""

from dataclasses import dataclass
from ....src.healthcheck.annotation_metrics import PageAnnotationComparison, AREA_BOUNDS

@dataclass
class PageExpectedValuePrinter:
    """ Utility class for displaying the page on which some metric falls outside of the expected range """

    comparison: PageAnnotationComparison

    def __init__(self, comparison: PageAnnotationComparison):
        self.comparison = comparison

    def print_count_mismatch(self) -> str:
        return f"page {self.comparison.page}: expected={self.comparison.expected_count} actual={self.comparison.cosmos_count}"

    def print_average_precision_mismatch(self) -> str:
        return f"page {self.comparison.page}: expected={AREA_BOUNDS} actual={self.comparison.average_precision}"

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

class DocumentAveragePrecisionPrinter:
    """ Utility class for displaying all pages in a document for which the annotation count falls outside of the expected range """
    page_values: list[PageExpectedValuePrinter]
    comparison_type: str

    def __init__(self, comparison_type, page_values: list[PageAnnotationComparison]):
        self.page_values = [PageExpectedValuePrinter(p) for p in page_values if not p.average_precision >= AREA_BOUNDS[0]]
        self.comparison_type = comparison_type

    def ok(self):
        return len(self.page_values) == 0
    
    def error_message(self):
        return f"Incorrect {self.comparison_type} on {len(self.page_values)} page(s)\n" + "\n".join([m.print_average_precision_mismatch() for m in self.page_values])
