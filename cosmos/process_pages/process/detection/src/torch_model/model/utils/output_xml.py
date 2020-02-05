from mrcnn.config import Config
from PIL import Image, ImageDraw
from mrcnn import utils
import mrcnn.model as modellib
from mrcnn.model import log
from dataset import PageDataset
import numpy as np
from tqdm import tqdm
from config import PageConfig
from random import sample
from argparse import ArgumentParser
from model2xml import model2xml
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

for idx, image_id in enumerate(tqdm(image_ids)): 
	# Load image and ground truth data
	image, image_meta, gt_class_id, gt_bbox, gt_mask =\
        modellib.load_image_gt(data_test, inference_config,image_id, use_mini_mask=False) 
	results = model.detect([image], verbose=0)
	r = results[0]
	info = data_test.image_info[image_id]
	model2xml(info["name"], ".", [1920, 1920], r["rois"], data_test.class_names)
