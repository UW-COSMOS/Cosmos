"""
get the centers of a map and draw them back to an image
"""
import sys
from utils.generate_anchors import generate_anchors
from PIL import Image
from PIL import ImageDraw
r = 1
image_size = 500
map_size = 16
feat_stride = float(image_size)/map_size
ratios = [1.0, 2.0, 0.5]
scales = [32, 64, 128]

base_image = Image.new("RGB", (image_size, image_size), color="#FFF")
centers, anchors = generate_anchors(feat_stride, map_size, ratios, scales, output_centers=True)
draw = ImageDraw.Draw(base_image)
centers = centers.reshape(-1, 2)
print(anchors)

for x,y in centers.astype(int):
    draw.ellipse((x-r, y-r, x+r, y+r), fill="#f00")
i,j = 6,6
for idx in range(9):
    x, y, h, w = anchors[i, j, idx]
    x1 = x - w/2
    y1 = y - h/2
    x2 = x + w/2
    y2 = y + h/2
    draw.rectangle((x1, y1, x2, y2), outline="#00f")
base_image.save("out.png", "PNG")



