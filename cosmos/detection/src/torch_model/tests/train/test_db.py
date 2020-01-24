import unittest
from train.data_layer.sql_types import ImageDB, Example, Neighbor
import torch


class TestImageDB(unittest.TestCase):
    def setUp(self):
        self.db = ImageDB.build()
        self.db = self.db

    def fill(self, n):
        for i in range(n):
            window = torch.rand(3,3)
            bbox = torch.rand(1,4)
            ex = Example(doc_id=i, page_id=1,object_id=1,bbox=bbox, window=window, label="cat")
            self.db.add(ex)
            ex = Example(doc_id=i, page_id=1,object_id=2,bbox=bbox, window=window, label="cat")
            self.db.add(ex)

    def test_init(self):
        """
        Simply start and insert data, checks that things are actually running
        """
        self.fill(10)
        print("done inserting, printing")
        for ex in self.db.query(Example):
            print(ex)
    
    def test_make_neighbors(self):
        self.fill(20)
        neighbor = Neighbor(center_doc_id=1,
                center_page_id=1,
                center_object_id=1,
                neighbor_doc_id=1,
                neighbor_page_id=1,
                neighbor_object_id=2)
        self.db.add(neighbor)
        self.db.commit()
        res = self.db.query(Example).filter(Example.doc_id == 1).all()
        flag = False
        for pair in res:
            if len(pair.neighbors) >0 :
                flag = True
                print(pair.neighbors[0].neighbor)
        self.assertTrue(flag)



if __name__ == "__main__":
    unittest.main()
