from PIL import Image
from PIL import ImageDraw
image_size = 500
base_image = Image.new("RGB", (image_size, image_size), color="#FFF")
box_1 = [256.0312,  31.9676,  64.0118, 128.0054]
box_2 = [256.5000,  71.2449, 147.0000, 183.0000]
draw = ImageDraw.Draw(base_image)


def bbox_to_coords(bbox):
    x1, y1, h, w = bbox
    x = x1 - w/2
    y = y1 - h/2
    return x, y, x1+w/2, y1+h/2


draw.rectangle(bbox_to_coords(box_1), outline="#f00")
draw.rectangle(bbox_to_coords(box_2), outline="#00f")
base_image.save("overlap.png", "PNG")
