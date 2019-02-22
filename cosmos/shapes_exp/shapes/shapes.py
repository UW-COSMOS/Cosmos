from PIL import ImageDraw
from collections import namedtuple

class ShapeNotInDBError(Exception):
    pass

def draw_square(im, center, width,noise):
    draw = ImageDraw.Draw(im)
    x,y = center
    noise_x, noise_y = noise
    x0 = x - width / 2 - noise_x
    x1 = x + width / 2 - noise_x
    y0 = y - width / 2 - noise_y
    y1 = y + width / 2 - noise_y
    draw.rectangle((x0,y0,x1,y1), outline="#000", fill="#f00")

def draw_circle(im, center, width,noise):
    draw = ImageDraw.Draw(im)
    x, y = center
    noise_x,noise_y = noise
    x0 = x - width / 2 -noise_x
    x1 = x + width / 2 - noise_x
    y0 = y - width / 2 - noise_y
    y1 = y + width / 2 - noise_y
    draw.ellipse((x0, y0, x1, y1), fill="#f00")

def draw_triangle(im, center, width,noise):
    draw = ImageDraw.Draw(im)
    x,y = center
    noise_x, noise_y = noise
    top = (x- noise_x, y-width/2 - noise_y)
    left =(x-width/2 - noise_x, y+width/2 - noise_y)
    right = (x+width/2 - noise_x, y+width/2 - noise_y)
    draw.polygon((top,left,right), fill="#f00")

class ShapesDB:
    def __init__(self):
        self.db = {
            "square": draw_square,
            "circle": draw_circle,
            "triangle": draw_triangle
        }

    def exists(self, shape):
        return shape in self.db

    def get_draw(self,shape):
        if not self.exists(shape):
            raise ShapeNotInDBError(f"shape: {shape} does not exist in the image db")
        return self.db[shape]