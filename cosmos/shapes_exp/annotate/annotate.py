from pascal_voc_writer import Writer

class ShapesAnnotate:

    def __init__(self,scene):
        self.scene = scene
        self.config = scene.config
        self.grid = scene.get_scene()

    def get_bbox(self, col, row, size, noise_x, noise_y):
        grid_h = self.scene.grid_height
        grid_w = self.scene.grid_width
        cx = grid_w * col + (grid_w / 2)
        cy = grid_h * row + (grid_w / 2)
        x0 = cx - size/2 - noise_x
        x1 = cx + size/2 - noise_x
        y0 = cy - size/2 - noise_y
        y1 = cy + size/2 - noise_y
        return int(x0), int(y0), int(x1), int(y1)

    def annotate(self,out):
        writer = Writer(out, self.scene.img_size, self.scene.img_size)
        for pt in self.grid.itertuples():
            x0, y0, x1, y1 = self.get_bbox(pt.col, pt.row, pt.size, pt.noise_x, pt.noise_y)
            writer.addObject(pt.shape, x0, y0, x1, y1)

        writer.save(out)

