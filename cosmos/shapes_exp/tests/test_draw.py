from scene.scene import Scene
from scene.config import YAMLReader
from draw.draw import ShapeDraw
import unittest
from os.path import isfile


class TestDrawing(unittest.TestCase):
    def setUp(self):
        self.config = YAMLReader("config.yaml")
        self.scene = Scene(self.config)
        self.scene.build()
        self.out = "out.png"

    def test_init(self):
        drawer = ShapeDraw(self.scene)
        self.assertIsNotNone(drawer)

    def test_draw(self):
        drawer = ShapeDraw(self.scene)
        drawer.draw(self.out)
        self.assertEqual(True, isfile(self.out))


