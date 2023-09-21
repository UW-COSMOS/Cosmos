import os
import glob
import configparser
import requests
import time
import xmltodict
import pandas as pd
from zipfile import ZipFile
from dataclasses import dataclass

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__),'../..'))

Rectangle = tuple[int,int,int,int]

def equals_comparator(expected, actual):
    return expected == actual

def in_bounds_comparator(expected, actual):
    return actual >= expected[0] and actual <= expected[1]

@dataclass
class PageExpectedValue:
    page: int
    expected: float
    actual: float

    def __str__(self) -> str:
        return f"page {self.page}: expected={self.expected} actual={self.actual}"

class DocumentExpectedValues:
    page_values: list[PageExpectedValue]


    def __init__(self, page_values):
        self.page_values = page_values

    def ok(self):
        return len(self.page_values) == 0
    
    def error_message(self):
        return f"Failed {len(self.page_values)} page metrics\n" + "\n".join([str(m) for m in self.page_values])


@dataclass
class PageAnnotationComparison:

    expected_count: int
    cosmos_count: int

    expected_area: int
    cosmos_area: int
    overlapping_area: int

    @property
    def area_ratio(self):
        """ The total area marked by cosmos compared to the total expected area"""
        # avoid dividing by zero
        if self.expected_area == 0 and self.cosmos_area == 0:
            return 1
        elif self.expected_area == 0 and self.cosmos_area > 0:
            return float('inf')
        else:
            return self.cosmos_area / float(self.expected_area)

    @property
    def overlap_percent(self):
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


class DocumentAnnotationComparison:

    page_comparisons: list[PageAnnotationComparison]

    def __init__(self, page_comparisons):
        self.page_comparisons = page_comparisons

    @property
    def expected_counts(self):
        return [p.expected_count for p in self.page_comparisons]

    @property
    def cosmos_counts(self):
        return [p.cosmos_count for p in self.page_comparisons]

    @property
    def expected_area_per_page(self):
        return [p.expected_area for p in self.page_comparisons]

    @property
    def cosmos_area_per_page(self):
        return [p.cosmos_area for p in self.page_comparisons]

    @property
    def overlap_area_per_page(self):
        return [p.overlapping_area for p in self.page_comparisons]

    @property
    def area_ratios(self):
        return [p.area_ratio for p in self.page_comparisons]

    @property
    def overlap_percents(self):
        return [p.overlap_percent for p in self.page_comparisons]

    def get_failures_per_page(self, expected, actual, meets_cond):
        failures = [PageExpectedValue(i+1,e,a) for i, (e,a) in enumerate(zip(expected, actual)) if not meets_cond(e,a)]
        return DocumentExpectedValues(failures)


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
        
        self._poll_for_cosmos_output()


    def _poll_for_cosmos_output(self):
        """ Poll the Cosmos Status endpoint for the input PDF, then download output upon completion """
        poll_interval = int(self.config['cosmos']['POLL_INTERVAL'])
        poll_count = int(self.config['cosmos']['MAX_POLL_COUNT'])
        status_endpoint, results_endpoint = self._submit_pdf_to_cosmos()

        job_done = False

        for i in range(poll_count):
            response = requests.get(status_endpoint)
            response_data = response.json()
            print(f"Polled status endpoint {i} times:\n{response_data}")
            job_done = response_data['error'] or response_data['job_completed']
            if job_done:
                break
            time.sleep(poll_interval)

        if not job_done or response_data['error']:
            raise ValueError("Unable to retrieve COSMOS output")

        with open(self._get_cosmos_path(), 'wb') as writer:
            writer.write(requests.get(results_endpoint).content)

    def _submit_pdf_to_cosmos(self):
        """ Submit the input pdf to the online COSMOS service """
        submit_endpoint = self.config['cosmos']['BASE_URL'] + '/process/'
        with open(self._get_pdf_path(), 'rb') as pdf_to_parse:
            file_form = {'pdf': pdf_to_parse }
            data_form = {'compress_images': False }
            response = requests.post(submit_endpoint, files=file_form, data=data_form)
            response_data = response.json()

            status_endpoint = response_data['status_endpoint']
            results_endpoint = response_data['result_endpoint']
            return (status_endpoint, results_endpoint)


    def _rectangle_intersection(self, r1: Rectangle, r2: Rectangle):
        """ Get the intersecting area of two rectangles """
        # via https://stackoverflow.com/a/27162334
        dx = min(r1[2], r2[2])- max(r1[0],r2[0])
        dy = min(r1[3], r2[3]) - max(r1[1],r2[1])

        return dx * dy if dx > 0 and dy > 0 else 0

    def _rectangle_area(self, r1: Rectangle):
        """ Get the area of a rectangle """
        length = r1[2] - r1[0]
        width = r1[3] - r1[1]
        return length * width

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
        
        return PageAnnotationComparison(
            expected_count= len(manual_annotations), 
            cosmos_count=len(cosmos_annotations),
            expected_area=sum(self._rectangle_area(m['bounding_box']) for m in manual_annotations),
            cosmos_area=sum(self._rectangle_area(a['bounding_box']) for a in cosmos_annotations),
            overlapping_area = sum(self._rectangle_intersection(c['bounding_box'], m['bounding_box']) 
                                for c in cosmos_annotations for m in manual_annotations)
        )


    def compare_pages_for_label(self, label_class):
        page_count = max(a['page_num'] for a in self.manual_annotations)
        page_comparisons = [
            self._compare_area_bounds_per_page(label_class, page_num)
            for page_num in range(1, page_count+1) # 1-indexed
        ]

        return DocumentAnnotationComparison(page_comparisons)

        

            
        


