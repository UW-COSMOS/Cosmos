from utils.bbox import BBoxes
from utils.bbox import stack_bboxes
import torch
import unittest


class TestBBoxes(unittest.TestCase):

    def setUp(self):
        self.tensor = torch.tensor([[0,0,20,20]])
        self.fmt = "xyxy"

    def test_init(self):
        bbox = BBoxes(self.tensor, self.fmt)
        self.assertIsNotNone(bbox)

    def test_addition(self):
        bbox = BBoxes(self.tensor, self.fmt)
        twoBox = bbox + bbox
        expect = BBoxes(torch.tensor([[0, 0, 40, 40]]), "xyxy")
        self.assertEqual(twoBox, expect)

    def test_change_fmt(self):
        bbox = BBoxes(self.tensor, self.fmt)
        bbox.change_format("xyhw")
        expect = BBoxes(torch.tensor([[10, 10, 20, 20]]), "xyhw")
        self.assertEqual(bbox, expect)
        bbox.change_format("xyhw")
        self.assertEqual(bbox,expect)
        bbox.change_format("xyxy")

        expect = BBoxes(self.tensor, self.fmt)
        self.assertEqual(bbox, expect)

    def test_stack(self):
        bbox = BBoxes(self.tensor, self.fmt)
        bbox2 = BBoxes(self.tensor, self.fmt)
        new_box = stack_bboxes((bbox, bbox2))
        self.assertIsNotNone(new_box)

    def test_shape(self):
        bbox = BBoxes(self.tensor, self.fmt)
        self.assertEqual(bbox.shape[0],1)
