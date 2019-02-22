from scene.scene import Scene
from scene.config import YAMLReader
import unittest
import pandas as pd

class TestScene(unittest.TestCase):
    def setUp(self):
        self.config = YAMLReader("config.yaml")

    def test_init(self):
        """
        Ensure that the object initializes with no issues
        :return:
        """
        scene = Scene(self.config)
        self.assertIsNotNone(scene)

    def test_make_grid(self):
        """
        Ensure that the object builds grids as expected
        :return:
        """
        scene = Scene(self.config)
        expect = self.config.GRID_COLS * self.config.GRID_ROWS
        grid = scene.grid
        self.assertEqual(expect, grid.shape[0])
        shape_expect = [Scene.nil_shape] *expect
        shape_expect = pd.Series(shape_expect)
        equality = shape_expect == grid["shape"]
        self.assertEqual(True, equality.all())


    def test_sample_shape(self):
        """
        test that shape sampling happens as expected
        by using KL divergence from the steady state
        :return:
        """
        pass

    def test_sample_size(self):
        """
        Test that size sampling is working correctly by using
        KL divergence from the uniform distribution
        :return:
        """
        pass

    def test_build_scene(self):
        """
        test that scene building happens as expected
        and that data structures are the correct shape etc
        :return:
        """
        scene = Scene(self.config)
        scene.build()
        grid = scene.grid
        expect = self.config.GRID_COLS * self.config.GRID_ROWS
        self.assertEqual(expect, grid.shape[0])
        shape_expect = [Scene.nil_shape] * expect
        shape_expect = pd.Series(shape_expect)
        equality = shape_expect != grid["shape"]
        self.assertEqual(True, equality.all())

