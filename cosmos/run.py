#!/usr/bin/env python3
"""
Script to run an end to end pipeline
"""

from UnicodeParser.parse_html_to_postgres import parse_html_to_postgres
import multiprocessing as mp
from argparse import ArgumentParser
import torch
from torch_model.model.model import MMFasterRCNN
from torch_model.model.utils.config_manager import ConfigManager
from torch_model.inference.inference import InferenceHelper 
from torch_model.inference.data_layer.inference_loader import InferenceLoader
import os
import subprocess
import re
from converters.model2xml import model2xml
from converters.xml2list import xml2list
from converters.list2html import list2html
from converters.pdf_extractor import parse_pdf
from tqdm import tqdm
import shutil
import preprocess.preprocess as pp
import postprocess.postprocess as post
from utils.voc_utils import ICDAR_convert
from connected_components.connected_components import write_proposals
from proposal_matcher.process import process_doc
from config import ingestion_settings

# PDF directory path

parser = ArgumentParser(description="Run the classifier")
parser.add_argument("pdfdir", type=str, help="Path to directory of PDFs")
parser.add_argument('-d', "--weightsdir", default='weights', type=str, help="Path to weights dir")
parser.add_argument('-w', "--weights", type=str, help='Path to weights file', required=True)
parser.add_argument('-t', "--threads", default=160, type=int, help="Number of threads to use")
parser.add_argument('-n', "--noingest", help="Ingest html documents and create postgres database", action='store_true')
parser.add_argument('-o', "--output", default='./', help="Output directory")
parser.add_argument('-p', "--tmp_path", default='tmp', help="Path to directory for temporary files")
parser.add_argument('--debug', help="Ingest html documents and create postgres database", action='store_true')

args = parser.parse_args()

# Path variables
model_config = "torch_model/model_config.yaml"
weights = args.weights
tmp = args.tmp_path
xml = os.path.join(args.output, "xml")
html = os.path.join(args.output, "html")
img_d = os.path.join(tmp, 'images2')

# Define and create required paths
req_paths = [tmp, f'{tmp}/images', f'{tmp}/images2', f'{tmp}/cc_proposals']
for path in req_paths:
    if not os.path.exists(path):
        os.makedirs(path)

# do pdfminer for all pdfs and preserve in mem
unicodes = {}
for pdf_name in os.listdir(args.pdfdir):
    print(os.path.join(args.pdfdir,pdf_name))
    df, limit = parse_pdf(os.path.join(args.pdfdir,pdf_name))
    unicodes[pdf_name] = (df, limit) 

# Convert a pdf into a set of images
def preprocess_pdfs(pdf_path):
    subprocess.run(['gs', '-dBATCH', '-dNOPAUSE', '-sDEVICE=png16m', '-dGraphicsAlphaBits=4',
                    '-dTextAlphaBits=4', '-r600', f'-sOutputFile="{tmp}/images/{pdf_path}_%d.png"', os.path.join(args.pdfdir, pdf_path)])

def resize_pngs(img_path):
    path, im = pp.resize_png(os.path.join(f'{tmp}', 'images', img_path))
    if path is not None:
        im.save(os.path.join(f'{tmp}', 'images', img_path))

def flatten_png(img_f):
    subprocess.run(['convert', '-flatten', os.path.join(f'{tmp}', 'images', img_f), os.path.join(f'{tmp}', 'images', img_f)])

def preprocess_pngs(img_f):
    pth, padded_img = pp.pad_image(os.path.join(f'{tmp}', 'images', img_f))
    if pth is not None:
        padded_img.save(os.path.join(img_d, img_f))

FILE_NAME = re.compile("(.*\.pdf)_([0-9]+)\.png")

def convert_to_html(xml_f):
    xpath = os.path.join(xml, xml_f)
    l = xml2list(xpath)
    pdf_name = FILE_NAME.search(f'{xml_f[:-4]}.png').group(1)
    list2html(l, f'{xml_f[:-4]}.png', img_d, html, os.path.join(f'{tmp}', 'images'), unicodes[pdf_name])

def match_proposal(proposal_f):
    proposal_f_full = os.path.join(f'{tmp}', proposal_f)
    xml_f = f'{xml}/{proposal_f[:-4]}' + '.xml'
    process_doc(xml_f, proposal_f_full, xml_f)

pool = mp.Pool(processes=args.threads)
results = [pool.apply_async(preprocess_pdfs, args=(x,)) for x in os.listdir(args.pdfdir)]
[r.get() for r in results]

results = [pool.apply_async(resize_pngs, args=(x,)) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
[r.get() for r in results]

print('Begin writing proposals')

results = [pool.apply_async(write_proposals, args=(os.path.join(f'{tmp}', 'images', x),), kwds={"output_dir" : os.path.join(tmp,"cc_proposals")}) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
[r.get() for r in results]


print('Begin preprocessing pngs')
results = [pool.apply_async(preprocess_pngs, args=(x,)) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
[r.get() for r in results]

with open('test.txt', 'w') as wf:
    for f in os.listdir(f'{tmp}/images'):
        wf.write(f[:-4] + '\n')

shutil.move('test.txt', f'{tmp}/test.txt')

model_config = ConfigManager(model_config)
model = MMFasterRCNN(model_config)
model.load_state_dict(torch.load(weights, map_location={"cuda:0": "cpu"}))
loader = InferenceLoader(f"{tmp}/images", f"{tmp}/cc_proposals", "png", model_config.WARPED_SIZE)
runner = InferenceHelper(model, loader, torch.device("cpu"))

runner.run(xml)
# for idx, image_id in enumerate(tqdm(image_ids)):
    # # Load image and ground truth data
    # image, image_meta, gt_class_id, gt_bbox, gt_mask = \
        # modellib.load_image_gt(data_test, inference_config,image_id, use_mini_mask=False)
    # results = model.detect([image], verbose=0)
    # r = results[0]
    # info = data_test.image_info[image_id]
    # zipped = zip(r["class_ids"], r["rois"])
    # model2xml(info["str_id"], xml, [1920, 1920], zipped, data_test.class_names, r['scores'])

# results = [pool.apply_async(match_proposal, args=(x,)) for x in os.listdir(f'{tmp}') if x[-4:] == '.csv']
# [r.get() for r in results]

if not os.path.exists(html):
    os.makedirs(html)
    os.makedirs(os.path.join(html, 'img'))
    os.makedirs(os.path.join(html, 'latex'))

print('Begin converting to html')
results = [pool.apply_async(convert_to_html, args=(x,)) for x in os.listdir(xml)]
[r.get() for r in results]

# postprocess
tmp_html = html.replace('html', 'html_tmp')
if not os.path.exists(tmp_html):
    os.makedirs(tmp_html)

print("Running postprocessing")
post.postprocess(html, tmp_html)

# replace old html with corrected stuff.
if not args.debug:
    shutil.rmtree(html)
    shutil.move(tmp_html, html)

print('Here')
# Parse html files to postgres db
input_folder = ingestion_settings['input_folder']
print(input_folder)

# intermediate folder location (will be auto-generated)
merge_folder = ingestion_settings['merge_folder']
output_html = ingestion_settings['output_html']
output_words = ingestion_settings['output_words']
output_equations = ingestion_settings['output_equations']

db_connect_str = ingestion_settings['db_connect_str']

strip_tags = ingestion_settings['strip_tags']
ignored_file_when_link = ingestion_settings['ignored_file_when_link']
output_csv = os.path.join(args.output, "output.csv")

if not args.noingest:
    parse_html_to_postgres(input_folder, output_html, merge_folder, output_words, output_equations, db_connect_str, strip_tags, ignored_file_when_link, output_csv, store_into_postgres=True)

if not args.debug:
    shutil.rmtree(f'{tmp}')
