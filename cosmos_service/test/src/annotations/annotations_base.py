import os
import glob
import configparser
import requests
import time
import xmltodict
import pandas as pd
from zipfile import ZipFile
from ..util.cosmos_client import submit_pdf_to_cosmos, poll_for_cosmos_output
from ....src.healthcheck.annotation_metrics import AnnotationComparator, AnnotationBounds
from .error_printer import DocumentExpectedCountPrinter, DocumentExpectedOverlapPrinter

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__),'../..'))

AREA_BOUNDS=(0.9,1.1)

class BaseAnnotationComparisonTest:
    pdf_name: str
    pdf_remote_url: str
    comparator: AnnotationComparator
    config: configparser.ConfigParser

    def _setup(self, pdf_name, pdf_remote_url = None):
        self.pdf_name = pdf_name
        self.pdf_remote_url = pdf_remote_url
        self.config = self._get_config()
        
        # Check that the expected inputs and outputs exist locally,
        # and download them if they don't
        self._get_pdf()
        self._get_cosmos_output()
        
        # Read in both the manually annotated xml files and cosmos generated annotations
        # as dictionaries with a common schema
        manual_annotations = self._load_annotation_xml()
        
        self.comparator = AnnotationComparator(pdf_name, ZipFile(self.cosmos_path), manual_annotations)

    def _get_config(self):
        """ Read test behavior configuration parameters from a config file """
        config = configparser.ConfigParser()
        config.read(os.path.join(BASE_DIR, 'config.ini'))
        return config
    
    @property
    def pdf_path(self):
        """ Get the expected location of the input PDF on disk """
        return os.path.join(BASE_DIR,'resources', 'pdfs', f'{self.pdf_name}.pdf')

    @property
    def cosmos_path(self):
        """ Get the expected location of the stored COSMOS output on disk """
        return os.path.join(BASE_DIR,'resources', 'cosmos_output', f'{self.pdf_name}.zip')


    def _get_pdf(self):
        """Confirm the existence of the source PDF, and download it if it doesn't exist"""
        if self.pdf_remote_url is None and not os.path.exists(self.pdf_path):
            # PDF doesn't exist and no way to attain it, error out
            raise ValueError(f"No PDF found at {self.pdf_path} and no remote URL given")
        
        elif os.path.exists(self.pdf_path) and (
                self.pdf_remote_url is None or self.config['cache'].getboolean('CACHE_PDFS')):
            # PDF exists and config enables using a cached PDF
            return
        
        else:
            with open(self.pdf_path, 'wb') as pdf_writer:
                pdf_writer.write(requests.get(self.pdf_remote_url).content)

    
    def _get_cosmos_output(self):
        """Confirm the existence of COSMOS output for the source PDF, and download it if it doesn't exist"""
        cosmos_path = self.cosmos_path
        if os.path.exists(cosmos_path) and self.config['cache'].getboolean('CACHE_COSMOS_OUTPUT'):
            # output already exists, return
            return
        
        status_endpoint, results_endpoint = submit_pdf_to_cosmos(self.pdf_path)
        poll_for_cosmos_output(status_endpoint, results_endpoint, self.cosmos_path)

    def _cosmos_obj_from_manual_obj(self, page_num, obj):
        """Convert the extracted XML of a manual annotation to use the same keys/data types as 
        those included in the cosmos parquet
        """
        bbox = obj['bndbox']
        bounds = [int(float(b)) 
            for b in [bbox['xmin'],bbox['ymin'],bbox['xmax'],bbox['ymax']]]
        
        return AnnotationBounds(
            page_num = page_num,
            postprocess_cls = obj['name'],
            bounding_box= bounds
        )

    def _load_annotation_xml(self):
        """ Read every annotation xml file from the directory corresponding to this test's pdf,
        assuming one xml file per article page
        """
        annotations_path = os.path.join(BASE_DIR, 'resources', 'annotated', self.pdf_name, '*.xml')
        annotations = []

        for xml_path in glob.glob(annotations_path):
            with open(xml_path) as xml_data:
                xml_dict = xmltodict.parse(xml_data.read())
                page_num = int(xml_dict['annotation']['page'])
                objects = xml_dict['annotation']['object']
                annotations.extend(self._cosmos_obj_from_manual_obj(page_num, obj) for obj in objects)
        return annotations


    def check_count_per_page(self, label_class):
        comparison = self.comparator.compare_pages_for_label(label_class)
        failures = DocumentExpectedCountPrinter(f"{label_class} count", comparison.page_comparisons)
        assert failures.ok(), failures.error_message()

    def check_overlap_per_page(self, label_class):
        comparison = self.comparator.compare_pages_for_label(label_class)
        failures = DocumentExpectedOverlapPrinter(f"{label_class} overlap percentage", comparison.page_comparisons)
        assert failures.ok(), failures.error_message()

    def check_document_count(self, label_class):
        comparison = self.comparator.compare_pages_for_label(label_class)
        assert comparison.count_in_bounds, \
            f"Incorrect {label_class} count: expected={comparison.document_expected_count} actual={comparison.document_cosmos_count}"

    def check_document_overlap(self, label_class):
        comparison = self.comparator.compare_pages_for_label(label_class)
        assert comparison.overlap_in_bounds, \
            f"Incorrect {label_class} bounds: expected={AREA_BOUNDS} actual={comparison.document_overlap_percent}"

