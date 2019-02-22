from shapes.shapes import ShapesDB
from itertools import product
import pandas as pd
import numpy as np
from scene.markov import MarkovSampler


class Scene:
    # we need a nil we can check equality on
    nil_shape = "nil"

    def __init__(self, config):
        self.config = config
        self.init_dist = pd.read_csv(config.INIT_PATH)
        self.transition_matrix = pd.read_csv(config.TRANSITION_PATH)
        self.shapes = self.init_dist.columns
        self.img_size = config.IMAGE_SIZE
        self.db = ShapesDB()
        for shape in self.shapes:
            if not self.db.exists(shape):
                raise ValueError(f"unexpected shape in configuration {shape}")
        self.grid = None
        self.nrows = self.config.GRID_ROWS
        self.ncols = self.config.GRID_COLS
        self.max_radii = 0
        self.grid_width = 0
        self.grid_height = 0
        self.build_grid()
        self.size_distribution = np.random.uniform


    def build_grid(self):

        rows = range(self.nrows)
        cols = range(self.ncols)
        positions = list(product(rows, cols))
        i,j = list(zip(*positions))
        self.grid = pd.DataFrame({
            "row": i,
            "col": j,
        })
        self.grid["shape"] = Scene.nil_shape
        self.grid["size"] = 0
        self.grid["noise_x"] = 0
        self.grid["noise_y"] = 0
        self.grid_width = float(self.img_size) / self.ncols
        self.grid_height = float(self.img_size) / self.nrows
        self.max_radii = int(min(self.grid_width, self.grid_height))
        if self.max_radii < self.config.MAX_SIZE:
            raise ValueError(f"max size set to {self.config.MAX_SIZE}, but the columns only allow {self.max_radii}")

    def sample_size(self):
        return self.size_distribution(self.config.MIN_SIZE, self.config.MAX_SIZE, 1)

    def noise_position(self):
        return self.config.POSITION_NOISE * np.random.standard_normal(2)

    def get_scene(self):
        return self.grid

    def get_config(self):
        return self.config

    def build(self):
        for col in range(self.ncols):
            sub_col = self.grid[self.grid["col"] == col]
            markov_sampler = MarkovSampler(
                init_dist=self.init_dist,
                transition_matrix=self.transition_matrix)
            for loc in sub_col.index:
                self.grid.loc[loc, "shape"] = markov_sampler.sample()

                self.grid.loc[loc, "size"] = self.size_distribution(
                    self.config.MIN_SIZE,
                    self.config.MAX_SIZE)
                self.grid.loc[loc,["noise_x","noise_y"]] = self.noise_position()
