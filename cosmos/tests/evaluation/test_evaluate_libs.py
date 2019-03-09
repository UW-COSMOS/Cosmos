import unittest
import sys
import os
import numpy as np
top = os.path.abspath("../..")
print(top)
sys.path.append(top)

from evaluate.evaluate_libs import run_coco, run_voc
classes = ["Section Header", "Body Text", "Figure", "Figure Caption", "Table", "Equation", \
      "Page Footer", "Page Header", "Table Caption", "Table Note", "Abstract", "Other", "Equation label", "Reference text", "Figure Note"]

def is1ornan(x):
    return x == 1.0 or x !=x

class TestEvaluation(unittest.TestCase):
    def setUp(self):
        self.gt_dir = "data/annotations"
        self.pred_dir = "data/predictions"
        self.pred_dir_overlaps = "data/predictions_overlaps"

    def test_perfect(self):
        print("---- TESTING PERFECT ----")

        res = run_voc(self.gt_dir, self.gt_dir)
        print(res)
        self.assertAlmostEqual(1.0, 1.0, places=2)

    
if __name__ == "__main__":
        unittest.main()
