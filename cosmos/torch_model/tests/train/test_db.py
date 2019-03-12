import unittest
from train.data_layer.sql_types import ImageDB, Example, Neighbor
import torch


class TestImageDB(unittest.TestCase):
    def setUp(self):
        self.db = ImageDB()
        self.db = self.db()

    def test_init(self):
        """
        Simply start and insert data, checks that things are actually running
        """
        for i in range(10):
            window = torch.rand(3,3)
            bbox = torch.rand(1,4)
            ex = Example(doc_id=i, page_id=i,object_id=i,bbox=bbox, window=window, label="cat")
            self.db.add(ex)
        print("done inserting, printing")
        for ex in self.db.query(Example):
            print(ex)



if __name__ == "__main__":
    unittest.main()
