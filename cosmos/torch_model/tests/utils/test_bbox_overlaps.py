from PIL import Image
from PIL import ImageDraw
from utils.bbox_overlaps import bbox_overlaps
import torch
image_size = 500
base_image = Image.new("RGB", (image_size, image_size), color="#FFF")
boxes = [
    [15,15,28,28],
    [45,45,10,10],
    [30,30, 30,30]
]
gt_boxes = [[15, 15, 30,30],
            [45,45,15,14]]
draw = ImageDraw.Draw(base_image)


def bbox_to_coords(bbox):
    x1, y1, h, w = bbox
    x = x1 - w/2
    y = y1 - h/2
    return x, y, x1+w/2, y1+h/2


for box in gt_boxes:
    draw.rectangle(bbox_to_coords(box), outline="#f00")

for box in boxes:
    draw.rectangle(bbox_to_coords(box), outline="#00f")

base_image.save("overlap.png", "PNG")
bboxes = torch.tensor(boxes).float()
gt_boxes = torch.tensor(gt_boxes).float()
out = bbox_overlaps(bboxes, gt_boxes)
print(out)
print(out.shape)

