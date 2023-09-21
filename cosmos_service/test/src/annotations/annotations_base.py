import os
import glob
import configparser
import requests
import time
import xmltodict
import pandas as pd
from zipfile import ZipFile
from ..util.cosmos_client import submit_pdf_to_cosmos, poll_for_cosmos_output
from .page_metrics import *

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__),'../..'))

AREA_BOUNDS=(0.9,1.1)

class BaseAnnotationComparisonTest:
    pdf_name: str
    pdf_remote_url: str
    config: configparser.ConfigParser
    manual_annotations: dict
    cosmos_annotations: dict
    # TODO this special-casing for equations is a bit clunky
    cosmos_equation_annotations: dict

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
        self.manual_annotations = self._load_annotation_xml()
        self.cosmos_annotations = self._load_cosmos_parquet(f'{self.pdf_name}.parquet')
        self.cosmos_equation_annotations = self._load_cosmos_parquet(f'{self.pdf_name}_equations.parquet')

    def _get_config(self):
        """ Read test behavior configuration parameters from a config file """
        config = configparser.ConfigParser()
        config.read(os.path.join(BASE_DIR, 'config.ini'))
        return config
    
    def _get_pdf_path(self):
        """ Get the expected location of the input PDF on disk """
        return os.path.join(BASE_DIR,'resources', 'pdfs', f'{self.pdf_name}.pdf')

    def _get_cosmos_path(self):
        """ Get the expected location of the stored COSMOS output on disk """
        return os.path.join(BASE_DIR,'resources', 'cosmos_output', f'{self.pdf_name}.zip')


    def _get_pdf(self):
        """Confirm the existence of the source PDF, and download it if it doesn't exist"""
        pdf_path = self._get_pdf_path()
        if self.pdf_remote_url is None and not os.path.exists(pdf_path):
            # PDF doesn't exist and no way to attain it, error out
            raise ValueError(f"No PDF found at {pdf_path} and no remote URL given")
        
        elif os.path.exists(pdf_path) and (
                self.pdf_remote_url is None or self.config['cache'].getboolean('CACHE_PDFS')):
            # PDF exists and config enables using a cached PDF
            return
        
        else:
            with open(pdf_path, 'wb') as pdf_writer:
                pdf_writer.write(requests.get(self.pdf_remote_url).content)

    
    def _get_cosmos_output(self):
        """Confirm the existence of COSMOS output for the source PDF, and download it if it doesn't exist"""
        cosmos_path = self._get_cosmos_path()
        if os.path.exists(cosmos_path) and self.config['cache'].getboolean('CACHE_COSMOS_OUTPUT'):
            # output already exists, return
            return
        
        status_endpoint, results_endpoint = submit_pdf_to_cosmos(self._get_pdf_path())
        poll_for_cosmos_output(status_endpoint, results_endpoint)

    def _cosmos_obj_from_manual_obj(self, page_num, obj):
        """Convert the extracted XML of a manual annotation to use the same keys/data types as 
        those included in the cosmos parquet
        """
        bbox = obj['bndbox']
        bounds = [int(float(b)) 
            for b in [bbox['xmin'],bbox['ymin'],bbox['xmax'],bbox['ymax']]]
        
        return {
            'page_num': page_num,
            'postprocess_cls': obj['name'],
            'bounding_box': bounds
        }


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


    def _load_cosmos_parquet(self, parquet_file):
        """Extract the given parquet file from the cosmos output zip, then convert it to a dict"""
        with ZipFile(self._get_cosmos_path()) as zipf:
            with zipf.open(parquet_file) as parquet_file:
                df = pd.read_parquet(parquet_file)
                return df.to_dict('records')


    def _get_labeled_item_per_page(self, annotations, label_class, page):
        return [a for a in annotations if a['postprocess_cls'] == label_class and a['page_num'] == page]

    def _compare_area_bounds_per_page(self, label_class, page):
        manual_annotations = self._get_labeled_item_per_page(self.manual_annotations, label_class, page) 
        cosmos_annotations = self._get_labeled_item_per_page(self.cosmos_annotations, label_class, page) 
        return PageAnnotationComparison.from_bounds(manual_annotations, cosmos_annotations)

    def compare_pages_for_label(self, label_class, comparison_type="metric"):
        page_count = max(a['page_num'] for a in self.manual_annotations)
        page_comparisons = [
            self._compare_area_bounds_per_page(label_class, page_num)
            for page_num in range(1, page_count+1) # 1-indexed
        ]

        return DocumentAnnotationComparison(page_comparisons)

    def check_count_per_page(self, label_class):
        fc = self.compare_pages_for_label(label_class)
        failures = fc.get_failures_per_page(
            f"{label_class} count", fc.expected_counts, fc.cosmos_counts, equals_comparator)
        assert failures.ok(), failures.error_message()

    def check_overlap_per_page(self, label_class):
        fc = self.compare_pages_for_label(label_class)
        failures = fc.get_failures_per_page(
            f"{label_class} overlap percentage",
            [AREA_BOUNDS for _ in fc.expected_counts],
            fc.overlap_percents, 
            in_bounds_comparator)
        assert failures.ok(), failures.error_message()
