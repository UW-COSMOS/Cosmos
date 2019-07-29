import pickle
import unittest
import pandas as pd
from src.pdf_table_extractions import extract_tables

class TestTableExtractions(unittest.TestCase):
    def easy_table():
        metadata = {'pdf_name': 'test/test_docs/foo1.pdf', 'page_num':'1', 'camelot_coords': '170, 370, 560, 270'}
    
        df1 = pickle.loads(extract_tables(metadata)[0]['table_df'])
        self.assertEqual((6,4), df1.shape)

    def medium_table():
        metadata = {'pdf_name': 'test/test_docs/foo2.pdf', 'page_num':'1', 'camelot_coords': '316, 499, 566, 337'}

        df2 = pickle.loads(extract_tables(metadata)[0]['table_df'])
        self.assertEqual((11,2), df2.shape)

    def hard_table():
        metadata = {'pdf_name': 'test/test_docs/foo3.pdf', 'page_num':'1', 'camelot_coords': '46, 704, 521, 546'}

        df3 = pickle.loads(extract_tables(metadata)[0]['table_df'])
        self.assertEqual((16,4), df3.shape)

if __name__ == '__main__':
    unittest.main()
