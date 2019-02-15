from mrcnn.config import Config
from PIL import Image, ImageDraw
from mrcnn import utils
import mrcnn.model as modellib
from mrcnn import visualize
from mrcnn.model import log
from dataset import PageDataset
import numpy as np
from mrcnn import visualize
from tqdm import tqdm
from config import PageConfig
from random import sample
from argparse import ArgumentParser

parser = ArgumentParser(description="create demo images from a test set")
parser.add_argument("data_path", type=str, help="path to test directory")
parser.add_argument("sample_size", type=int, help="number of images to generate")
parser.add_argument("model_path", type=str, help="path to the model weights")
args = parser.parse_args()
class InferenceConfig(Config):
	NAME = "pages"
	BACKBONE = "resnet50"
	GPU_COUNT = 1
	IMAGE_MAX_DIM = 1920
	RPN_ANCHOR_SCALES = (32,64, 256, 512,1024)
	NUM_CLASSES = 4
	IMAGES_PER_GPU = 1
def rect(d, im, color, points):
	for i, pt in enumerate(points[:-1]):
		d.line((pt, points[i+1]), fill=color, width=4)

def draw_rois(d, im, color, rois, cls):
	for idx, roi in enumerate(rois):
		y1, x1, y2, x2 = roi
		points = (x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)
                text_points = (x1, y1)
                d.text(text_point, cls[idx])
		rect(d, im, color, points )

def draw_image(name, path, rois,gt_rois, classes):
	im = Image.open(path)
	d = ImageDraw.Draw(im)
	draw_rois(d, im, "#f00", rois, classes)
	draw_rois(d, im, "#00f", gt_rois, classes)
	im.save(f"{name}.png", "png")

inference_config = InferenceConfig()
config = PageConfig()
model = modellib.MaskRCNN(mode="inference", 
                          config=inference_config,
                          model_dir=args.model_path)

model_path = model.find_last()

print("Loading weights from ", model_path)
model.load_weights(model_path, by_name=True)
data_test = PageDataset()
data_test.load_page(args.data_path, "test")
data_test.prepare()
image_ids = data_test.image_ids
image_ids = sample(list(image_ids), args.sample_size)
APs = dict([(cls, []) for cls in data_test.class_names])
ious = dict([(cls, []) for cls in data_test.class_names])

for idx, image_id in enumerate(tqdm(image_ids)): 
	# Load image and ground truth data
	image, image_meta, gt_class_id, gt_bbox, gt_mask =\
        modellib.load_image_gt(data_test, inference_config,image_id, use_mini_mask=False) 
	results = model.detect([image], verbose=0)
        print(r)
	r = results[0]
	info = data_test.image_info[image_id]
	draw_image(info["str_id"], info["path"], r["rois"], gt_bbox,None)
