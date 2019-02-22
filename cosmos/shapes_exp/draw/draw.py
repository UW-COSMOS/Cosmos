"""
Draw a PNG of a scene
Author: Josh McGrath
"""
from PIL import Image
from shapes.shapes import ShapesDB




class ShapeDraw:
    def __init__(self, scene):
        """
        Initialize a ShapeDraw object
        :param scene: Scene Object
        """
        self.grid = scene.get_scene()
        self.scene = scene
        self.config = scene.get_config()
        self.db = ShapesDB()

    def draw(self, out):
        size = self.config.IMAGE_SIZE
        im = Image.new("RGB", (size,size), color="#fff")
        for pt in self.grid.itertuples():
            center = self.get_center(pt.col, pt.row)
            noise_x = pt.noise_x
            noise_y = pt.noise_y
            draw_func = self.db.get_draw(pt.shape)
            draw_func(im, center, pt.size, (noise_x, noise_y))
        im.save(out)

    def get_center(self, col, row):
        grid_h = self.scene.grid_height
        grid_w = self.scene.grid_width
        x = grid_w*col + (grid_w / 2)
        y = grid_h*row + (grid_w / 2)
        return x, y


