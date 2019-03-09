import unittest
import sys
import os
import numpy as np
top = os.path.abspath("../..")
print(top)
sys.path.append(top)

from evaluate.evaluate_iccv import evaluate_single, evaluate_dir
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

        res = evaluate_dir(self.gt_dir, self.gt_dir, classes)
        prec = res.loc["total"]["precision"]
        self.assertAlmostEqual(1.0, prec, places=2)
        rec = res.loc["total"]["recall"]
        self.assertAlmostEqual(1.0, rec, places=2)

    def test_imperfect(self):
        """
        some body text labels have been changed to figure
        figure recall should be perfect, body text precision should be too
        body text recall should be less than perfect
        """
        print("---- TESTING CLASS CONFUSION ----")
        res = evaluate_dir(self.pred_dir, self.gt_dir, classes)
        for cls in classes:
            if cls == "Figure":
                self.assertNotEqual(1.0, res.loc[cls]["precision"])
                self.assertAlmostEqual(1.0, res.loc[cls]["recall"], places=2)
            elif cls == "Body Text":
                self.assertNotEqual(1.0, res.loc[cls]["recall"])
                self.assertAlmostEqual(1.0, res.loc[cls]["precision"], places=2)
            else:
                self.assertTrue(is1ornan(res.loc[cls]["precision"]))
                self.assertTrue(is1ornan(res.loc[cls]["recall"]))

    def test_overlap_filtering(self):
        """
        some body text classes have been replicated, which should cause precision
        to go down
        """
        print("---- TESTING OVERLAP FILTERING ----")
        res = evaluate_dir(self.pred_dir_overlaps, self.gt_dir, classes)
        self.assertNotEqual(1.0, res.loc["Body Text"]["precision"])


if __name__ == "__main__":
        unittest.main()
