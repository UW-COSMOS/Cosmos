#!/usr/bin/env python3
"""
Script to run an end to end pipeline
"""

from mrcnn.config import Config
from argparse import ArgumentParser
import mrcnn.model as modellib
from config import PageConfig
from dataset import PageDataset
import os
import subprocess
from model2xml import model2xml
from xml2list import xml2list
from list2html import list2html
from tqdm import tqdm
import shutil

# PDF directory path

parser = ArgumentParser(description="Run the classifier")
parser.add_argument("pdfdir", type=str, help="Path to directory of PDFs")
parser.add_argument('-w', "--weightsdir", default='weights', type=str, help="Path to weights file")

args = parser.parse_args()
if not os.path.exists('tmp'):
    os.makedirs('tmp')
    os.makedirs('tmp/images')
for pdf_f in os.listdir(args.pdfdir):
    subprocess.run(['convert', '-density', '150', '-trim', os.path.join(args.pdfdir, pdf_f), '-quality', '100',
                    '-sharpen', '0x1.0', os.path.join('tmp','images', f'{pdf_f}-%04d.png')])
for img_f in os.listdir('tmp/images'):
    subprocess.run(['convert', '-flatten', os.path.join('tmp', 'images', img_f), os.path.join('tmp', 'images', img_f)])

with open('test.txt', 'w') as wf:
    for f in os.listdir('tmp/images'):
        wf.write(f[:-4] + '\n')

shutil.move('test.txt', 'tmp/test.txt')


class InferenceConfig(Config):
    NAME = "pages"
    BACKBONE = "resnet50"
    GPU_COUNT = 1
    IMAGE_MAX_DIM = 1920
    RPN_ANCHOR_SCALES = (32,64, 256, 512,1024)
    NUM_CLASSES = 5
    IMAGES_PER_GPU = 1

inference_config = InferenceConfig()
config = PageConfig()
model = modellib.MaskRCNN(mode="inference",
                          config=inference_config,
                          model_dir=args.weightsdir)
model_path = model.find_last()
print("Loading weights from ", model_path)
model.load_weights(model_path, by_name=True)
data_test = PageDataset('test', 'tmp', 0, nomask=True)
data_test.load_page(classes=['Figure', 'Table', 'Equation', 'Body Text'])
data_test.prepare()
image_ids = data_test.image_ids

if not os.path.exists('xml'):
    os.makedirs('xml')

for idx, image_id in enumerate(tqdm(image_ids)):
    # Load image and ground truth data
    image, image_meta, gt_class_id, gt_bbox, gt_mask = \
        modellib.load_image_gt(data_test, inference_config,image_id, use_mini_mask=False)
    results = model.detect([image], verbose=0)
    r = results[0]
    info = data_test.image_info[image_id]
    zipped = zip(r["class_ids"], r["rois"])
    model2xml(info["str_id"], 'xml', [1920, 1920], zipped, data_test.class_names)

for xml_f in os.listdir('xml'):
    xpath = os.path.join('xml', xml_f)
    l = xml2list(xpath)
    list2html(l, f'{xml_f[:-4]}.png', 'tmp/images', 'html')
    
shutil.rmtree('xml')
shutil.rmtree('tmp')



