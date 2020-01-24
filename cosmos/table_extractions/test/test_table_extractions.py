import pickle
import unittest
import pandas as pd
from ..src.pdf_table_extractions import run_table_extraction

class TestTableExtractions(unittest.TestCase):
    tables = {}

    def create_test_metadata(self, db, buffer_size, tables_per_job):
        metadata1 = {'pdf_name': 'test/test_docs/foo1.pdf', 'page_num': '1', 'coords': '', 'camelot_coords': '170, 370, 560, 270'}
        metadata2 = {'pdf_name': 'test/test_docs/foo2.pdf', 'page_num': '1', 'coords': '', 'camelot_coords': '316, 499, 566, 337'}
        metadata3 = {'pdf_name': 'test/test_docs/foo3.pdf', 'page_num': '1', 'coords': '', 'camelot_coords': '46, 704, 521, 546'}

        return [metadata1, metadata2, metadata3]

    def insert_test_tables_local(self, coll, detected_tables):
        self.tables.update({detected_tables['pdf_name']: pickle.loads(detected_tables['table_df'])})

    def easy_table(self):
        df1 = self.tables['foo1.pdf']
        self.assertEqual((6, 4), df1.shape)

    def medium_table(self):
        df2 = self.tables['foo2.pdf']
        self.assertEqual((11, 2), df2.shape)

    def hard_table(self):
        df3 = self.tables['foo3.pdf']
        self.assertEqual((16, 4), df3.shape)

    def test_tables(self):
        run_table_extraction(self.create_test_metadata, self.insert_test_tables_local, 1, False)

        self.easy_table()
        self.medium_table()
        self.hard_table()

if __name__ == '__main__':
    unittest.main()
