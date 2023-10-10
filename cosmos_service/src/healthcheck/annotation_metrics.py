from zipfile import ZipFile
from .page_metrics import PageAnnotationComparison, DocumentAnnotationComparison, AnnotationBounds
import pandas as pd

AREA_BOUNDS=(0.9,1.1)
DEFAULT_REGION_TYPES = ["Figure", "Equation", "Table"]


class AnnotationComparator:
    cosmos_output: ZipFile
    pdf_name: str
    cosmos_annotations: list[AnnotationBounds]
    manual_annotations: list[AnnotationBounds]

    def __init__(self, cosmos_output : ZipFile, manual_annotations: list[AnnotationBounds]):
        self.cosmos_output = cosmos_output
        self.pdf_name = self._get_pdf_name()
        self.manual_annotations = manual_annotations
        self.cosmos_annotations = self._splice_equations_and_text_parquet()


    def _read_cosmos_parquet(self, parquet_file, bb_label='bounding_box'):
        """Extract the given parquet file from the cosmos output zip, then convert it to a dict"""
        with self.cosmos_output.open(parquet_file) as parquet_file:
            df = pd.read_parquet(parquet_file)
            return [
                {**data, 'bounding_box': [int(b) for b in data[bb_label]]}
                for data in df.to_dict('records')
            ]

    def _get_pdf_name(self):
        # Hack to find the naming pattern for parquet files inside the zip file
        equations_suffix = '_equations.parquet'
        equations_file = [f.filename for f in self.cosmos_output.filelist if f.filename.endswith(equations_suffix)][0]
        return equations_file.replace(equations_suffix, '')
    
    def _read_equations_parquet(self, parquet_file):
        equations_list = self._read_cosmos_parquet(parquet_file, 'equation_bb')
        for equation_dict in equations_list:
            equation_dict['postprocess_cls'] = 'Equation'
            equation_dict['page_num'] = equation_dict.pop('equation_page')
        
        return equations_list

    def _splice_equations_and_text_parquet(self):
        # TODO this special-casing for equations is a bit clunky
        cosmos_annotations = self._read_cosmos_parquet(f'{self.pdf_name}.parquet')
        equation_annotations = self._read_equations_parquet(f'{self.pdf_name}_equations.parquet')

        spliced_bounds = [
            *[c for c in cosmos_annotations if c['postprocess_cls'] != 'Equation'],
            *equation_annotations
        ]
        return [AnnotationBounds.parse_obj(sb) for sb in spliced_bounds]


    def _get_labeled_item_per_page(self, annotations: list[AnnotationBounds], label_class: str, page: int):
        return [a for a in annotations if a.postprocess_cls == label_class and a.page_num == page]

    def _compare_area_bounds_per_page(self, label_class: str, page: int):
        manual_annotations = self._get_labeled_item_per_page(self.manual_annotations, label_class, page) 
        cosmos_annotations = self._get_labeled_item_per_page(self.cosmos_annotations, label_class, page) 
        return PageAnnotationComparison.from_bounds(page, manual_annotations, cosmos_annotations)

    def compare_for_label(self, label_class: str):
        page_count = max(a.page_num for a in self.manual_annotations)
        page_comparisons = [
            self._compare_area_bounds_per_page(label_class, page_num)
            for page_num in range(1, page_count+1) # 1-indexed
        ]

        return DocumentAnnotationComparison(page_comparisons=page_comparisons, label_class=label_class)
