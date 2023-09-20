from .annotations_base import BaseAnnotationComparisonTest


PDF_NAME='sidarthe'
REMOTE_URL='https://www.nature.com/articles/s41591-020-0883-7.pdf'


class TestSidartheAnnotations(BaseAnnotationComparisonTest):



    def test_01_download_resources(self):
        self._setup(PDF_NAME, REMOTE_URL)
        self._get_pdf()
        self._get_cosmos_output()
