#!/usr/bin/env python3
"""
Script to run an end to end pipeline
"""

from Parser.parse_html_to_postgres import parse_html_to_postgres
import multiprocessing as mp
from mrcnn.config import Config
from argparse import ArgumentParser
import mrcnn.model as modellib
from config import PageConfig
from config import ingestion_settings
from dataset import PageDataset
import os
import subprocess
from model2xml import model2xml
from xml2list import xml2list
from list2html import list2html
from tqdm import tqdm
import shutil
import preprocess as pp
import postprocess
from voc_utils import ICDAR_convert
from connected_components.connected_components import write_proposals
from proposal_matcher.process import process_doc

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

tmp = args.tmp_path
xml = os.path.join(args.output, "xml")
html = os.path.join(args.output, "html")

req_paths = [tmp, f'{tmp}/images', f'{tmp}/images2', f'{tmp}/cc_proposals']
for path in req_paths:
    if not os.path.exists(path):
        os.makedirs(path)

img_d = os.path.join(tmp, 'images2')
def preprocess_pdfs(pdf_path):
    subprocess.run(['gs', '-dBATCH', '-dNOPAUSE', '-sDEVICE=png16m', '-dGraphicsAlphaBits=4',
                    '-dTextAlphaBits=4', '-r600', f'-sOutputFile="{tmp}/images/{pdf_path}_%d.png"', os.path.join(args.pdfdir, pdf_path)])
    #subprocess.run(['convert', '-density', '100', '-trim', os.path.join(args.pdfdir, pdf_path), '-quality', '100',
    #                '-sharpen', '0x1.0', os.path.join('tmp','images', f'{pdf_path}-%04d.png')])

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

def convert_to_html(xml_f):
    xpath = os.path.join(xml, xml_f)
    print('Begin convert')
    print(xpath)
    l = xml2list(xpath)
    print(l)
    list2html(l, f'{xml_f[:-4]}.png', img_d, html)

def match_proposal(proposal_f):
    proposal_f_full = os.path.join(f'{tmp}', proposal_f)
    xml_f = f'{xml}/{proposal_f[:-4]}' + '.xml'
    process_doc(xml_f, proposal_f_full, xml_f)
#for pdf_f in os.listdir(args.pdfdir):
#    subprocess.run(['gs', '-dBATCH', '-dNOPAUSE', '-sDEVICE=png16m', '-dGraphicsAlphaBits=4',
#                    '-dTextAlphaBits=4', '-r600', f'-sOutputFile="tmp/images2/{pdf_f}_%d.png"', os.path.join(args.pdfdir, pdf_f)])
#    subprocess.run(['convert', '-density', '150', '-trim', os.path.join(args.pdfdir, pdf_f), '-quality', '100',
#                    '-sharpen', '0x1.0', os.path.join('tmp','images', f'{pdf_f}-%04d.png')])
#for img_f in os.listdir('tmp/images'):
#    subprocess.run(['convert', '-flatten', os.path.join('tmp', 'images', img_f), os.path.join('tmp', 'images2', img_f)])

#shutil.rmtree('tmp/images')

pool = mp.Pool(processes=args.threads)
results = [pool.apply_async(preprocess_pdfs, args=(x,)) for x in os.listdir(args.pdfdir)]
[r.get() for r in results]

results = [pool.apply_async(resize_pngs, args=(x,)) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
[r.get() for r in results]

print('Begin writing proposals')
#[write_proposals(os.path.join('tmp', 'images', x)) for x in os.listdir(os.path.join('tmp', 'images'))]
results = [pool.apply_async(write_proposals, args=(os.path.join(f'{tmp}', 'images', x),), kwds={"output_dir" : tmp}) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
[r.get() for r in results]

#for img_f in os.listdir('tmp/images2'):
#    path, im = pp.resize_png(os.path.join('tmp', 'images2', img_f))
#    im.save(os.path.join('tmp', 'images2', img_f))
#    pth, padded_img = pp.pad_image(os.path.join('tmp', 'images2', img_f))
#    padded_img.save(os.path.join('tmp', 'images3', img_f))

print('Begin preprocessing pngs')
results = [pool.apply_async(preprocess_pngs, args=(x,)) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
[r.get() for r in results]

with open('test.txt', 'w') as wf:
    for f in os.listdir(f'{tmp}/images'):
        wf.write(f[:-4] + '\n')

shutil.move('test.txt', f'{tmp}/test.txt')


#class InferenceConfig(Config):
#    NAME = "pages"
#    BACKBONE = "resnet50"
#    GPU_COUNT = 1
#    IMAGE_MAX_DIM = 1920
#    RPN_ANCHOR_SCALES = (32,64, 256, 512,1024)
#    NUM_CLASSES = 5
#    IMAGES_PER_GPU = 1

class InferenceConfig(Config):
    NAME = "pages_uncollapsed"
    BACKBONE = "resnet50"
    GPU_COUNT = 1
    IMAGE_MAX_DIM = 1920
    RPN_ANCHOR_SCALES = (32,64, 256, 512,1024)
    NUM_CLASSES = 16
    IMAGES_PER_GPU = 1

inference_config = InferenceConfig()
config = PageConfig()
model = modellib.MaskRCNN(mode="inference",
                          config=inference_config,
                          model_dir=args.weightsdir)
#model_path = model.find_last()
#print("Loading weights from ", model_path)
model.load_weights(args.weights, by_name=True)
data_test = PageDataset('test', f'{tmp}', 0, nomask=True)
#data_test.load_page(classes=['Figure', 'Table', 'Equation', 'Body Text'])
data_test.load_page(classes=list(ICDAR_convert.keys()))
data_test.prepare()
image_ids = data_test.image_ids

if not os.path.exists(xml):
    os.makedirs(xml)

for idx, image_id in enumerate(tqdm(image_ids)):
    # Load image and ground truth data
    image, image_meta, gt_class_id, gt_bbox, gt_mask = \
        modellib.load_image_gt(data_test, inference_config,image_id, use_mini_mask=False)
    results = model.detect([image], verbose=0)
    r = results[0]
    info = data_test.image_info[image_id]
    zipped = zip(r["class_ids"], r["rois"])
    model2xml(info["str_id"], xml, [1920, 1920], zipped, data_test.class_names, r['scores'])

results = [pool.apply_async(match_proposal, args=(x,)) for x in os.listdir(f'{tmp}') if x[-4:] == '.csv']
[r.get() for r in results]

if not os.path.exists(html):
    os.makedirs(html)
    os.makedirs(os.path.join(html, 'img'))
    os.makedirs(os.path.join(html, 'latex'))
print('Begin converting to html')
results = [pool.apply_async(convert_to_html, args=(x,)) for x in os.listdir(xml)]
[r.get() for r in results]
print('End converting to html')
#for xml_f in os.listdir(xml):
#    xpath = os.path.join(xml, xml_f)
#    l = xml2list(xpath)
#    list2html(l, f'{xml_f[:-4]}.png', f'{tmp}/images', html)

# postprocess
tmp_html = html.replace('html', 'html_tmp')
if not os.path.exists(tmp_html):
    os.makedirs(tmp_html)

print("Running postprocessing")
postprocess.postprocess(html, tmp_html)

# replace old html with corrected stuff.
if not args.debug:
    shutil.rmtree(html)
    shutil.move(tmp_html, html)

# Parse html files to postgres db
input_folder = ingestion_settings['input_folder']

# intermediate folder location (will be auto-generated)
merge_folder = ingestion_settings['merge_folder']
output_html = ingestion_settings['output_html']
output_words = ingestion_settings['output_words']
output_equations = ingestion_settings['output_equations']

db_connect_str = ingestion_settings['db_connect_str']

strip_tags = ingestion_settings['strip_tags']
ignored_file_when_link = ingestion_settings['ignored_file_when_link']

if not args.noingest:
    parse_html_to_postgres(input_folder, output_html, merge_folder, output_words, output_equations, db_connect_str, strip_tags, ignored_file_when_link, store_into_postgres=True)

if not args.debug:
    shutil.rmtree(f'{tmp}')
