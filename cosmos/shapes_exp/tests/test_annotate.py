from scene.scene import Scene
from scene.config import YAMLReader
from annotate.annotate import ShapesAnnotate
import unittest
from os.path import isfile


class TestAnnotae(unittest.TestCase):
    def setUp(self):
        self.config = YAMLReader("config.yaml")
        self.scene = Scene(self.config)
        self.scene.build()
        self.out = "out.xml"

    def test_init(self):
        drawer = ShapesAnnotate(self.scene)
        self.assertIsNotNone(drawer)

    def test_draw(self):
        xml = ShapesAnnotate(self.scene)
        xml.annotate(self.out)
        self.assertEqual(True, isfile(self.out))
