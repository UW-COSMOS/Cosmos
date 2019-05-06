#!/usr/bin/env python3
"""
Script to run an end to end pipeline
"""

from infer.infer import run_inference
from UnicodeParser.parse_html_to_postgres import parse_html_to_postgres
from construct_caption_tables.construct import construct
import multiprocessing as mp
from argparse import ArgumentParser
import torch
from torch_model.model.model import MMFasterRCNN
from torch_model.model.utils.config_manager import ConfigManager
from torch_model.inference.inference import InferenceHelper
from torch_model.inference.data_layer.inference_loader import InferenceLoader
import sys
import os
import subprocess
import glob
import re
from converters.model2xml import model2xml
from converters.xml2list import xml2list
from converters.list2html import list2html
from converters.html2xml import htmlfile2xml
from converters.pdf_extractor import parse_pdf
from tqdm import tqdm
import shutil
import preprocess.preprocess as pp
import postprocess.postprocess as post
from utils.voc_utils import ICDAR_convert
from connected_components.connected_components import write_proposals
from proposal_matcher.process import process_doc
from config import ingestion_settings
import psycopg2
from postprocess.postprocess import group_cls

# PDF directory path

if __name__ == '__main__':
    parser = ArgumentParser(description="Run the classifier")
    parser.add_argument("pdfdir", type=str, help="Path to directory of PDFs")
    parser.add_argument('-d', "--device", default='cpu', type=str, help="Path to weights dir")
    parser.add_argument('-w', "--weights", type=str, help='Path to weights file', required=True)
    parser.add_argument('-t', "--threads", default=160, type=int, help="Number of threads to use")
    parser.add_argument('-n', "--noingest", help="Ingest html documents and create postgres database", action='store_true')
    parser.add_argument('-k', "--keep_pages", help="Keep the page-level PNGs", action='store_true')
    parser.add_argument('-o', "--output", default='./', help="Output directory")
    parser.add_argument('-p', "--tmp_path", default='tmp', help="Path to directory for temporary files")
    parser.add_argument('--debug', help="Ingest html documents and create postgres database", action='store_true')

    args = parser.parse_args()

    # Path variables
    model_config = "model_config.yaml"
    weights = args.weights
    device = args.device
    tmp = args.tmp_path
    xml = os.path.join(args.output, "xml")
    html = os.path.join(args.output, "html")
    img_d = os.path.join(tmp, 'images2')

    # Define and create required paths
    req_paths = [tmp, f'{tmp}/images', f'{tmp}/images2', f'{tmp}/cc_proposals', xml]
    for path in req_paths:
        if not os.path.exists(path):
            os.makedirs(path)

    # do pdfminer for all pdfs and preserve in mem
    unicodes = {}
    if os.listdir(args.pdfdir) == []:
        print("Input directory is empty! Exiting.")
        sys.exit(1)
    for pdf_name in os.listdir(args.pdfdir):
        if not pdf_name.endswith(".pdf"): continue
        print(os.path.join(args.pdfdir,pdf_name))
        df, limit = parse_pdf(os.path.join(args.pdfdir,pdf_name))
        unicodes[pdf_name] = (df, limit)

    # Convert a pdf into a set of images
    def preprocess_pdfs(pdf_path):
        if not pdf_path.endswith(".pdf"): return
        subprocess.run(['gs', '-dBATCH', '-dNOPAUSE', '-sDEVICE=png16m', '-dGraphicsAlphaBits=4',
                        '-dTextAlphaBits=4', '-r600', f'-sOutputFile="{tmp}/images/{pdf_path}_%d.png"', os.path.join(args.pdfdir, pdf_path)])

    def resize_pngs(img_path):
        path, im = pp.resize_png(os.path.join(f'{tmp}', 'images', img_path))
        if path is not None:
            im.save(os.path.join(f'{tmp}', 'images', img_path))
        print(os.path.join(f'{tmp}', 'images', img_path))

    def flatten_png(img_f):
        subprocess.run(['convert', '-flatten', os.path.join(f'{tmp}', 'images', img_f), os.path.join(f'{tmp}', 'images', img_f)])

    def preprocess_pngs(img_f):
        pth, padded_img = pp.pad_image(os.path.join(f'{tmp}', 'images', img_f))
        if pth is not None:
            padded_img.save(os.path.join(img_d, img_f))
        print(os.path.join(img_d, img_f))

    FILE_NAME = re.compile("(.*\.pdf)_([0-9]+)\.png")

    def convert_to_html(xml_f):
        xpath = os.path.join(xml, xml_f)
        l = xml2list(xpath)
        l = group_cls(l, 'Table', do_table_merge=True, merge_over_classes=['Figure', 'Section Header', 'Page Footer', 'Page Header'])
        l = group_cls(l, 'Figure')
        pdf_name = FILE_NAME.search(f'{xml_f[:-4]}.png').group(1)
        list2html(l, f'{xml_f[:-4]}.png', os.path.join(f'{tmp}', 'images'), html, unicodes[pdf_name] if pdf_name in unicodes else None)

    def update_xmls(html_f):
        hpath = os.path.join(html, html_f)
        htmlfile2xml(hpath, xml)

    pool = mp.Pool(processes=args.threads)

    print('Start preprocessing pdfs')
    if args.threads == 1:
        [preprocess_pdfs(x) for x in os.listdir(args.pdfdir)]
    else:
        results = [pool.apply_async(preprocess_pdfs, args=(x,)) for x in os.listdir(args.pdfdir)]
        [r.get() for r in results]
    print('End preprocessing pdfs')

    print('Begin resizing pngs')
    if args.threads == 1:
        [resize_pngs(x) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
    else:
        results = [pool.apply_async(resize_pngs, args=(x,)) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
        [r.get() for r in results]
    print('End resizing pngs')

    print('Begin writing proposals')
    if args.threads == 1:
        [write_proposals(os.path.join(f'{tmp}', 'images', x), output_dir=os.path.join(tmp,"cc_proposals")) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
    else:
        results = [pool.apply_async(write_proposals, args=(os.path.join(f'{tmp}', 'images', x),), kwds={"output_dir" : os.path.join(tmp,"cc_proposals")}) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
        [r.get() for r in results]
    print('End writing proposals')


    print('Begin preprocessing pngs')
    if args.threads == 1:
        [preprocess_pngs(x) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
    else:
        results = [pool.apply_async(preprocess_pngs, args=(x,)) for x in os.listdir(os.path.join(f'{tmp}', 'images'))]
        [r.get() for r in results]
    print('End preprocessing pngs')

    with open('test.txt', 'w') as wf:
        for f in os.listdir(f'{tmp}/images'):
            wf.write(f[:-4] + '\n')

    shutil.move('test.txt', f'{tmp}/test.txt')

    run_inference(f'{tmp}/images', f'{tmp}/cc_proposals', model_config, weights, xml, device)

    if not os.path.exists(html):
        os.makedirs(html)
        os.makedirs(os.path.join(html, 'img'))
        os.makedirs(os.path.join(html, 'latex'))

    print('Begin converting to html')
    if args.threads == 1:
        [convert_to_html(x) for x in os.listdir(xml)]
    else:
        results = [pool.apply_async(convert_to_html, args=(x,)) for x in os.listdir(xml)]
        [r.get() for r in results]
    print('End converting to html')

    # postprocess
    tmp_html = html.replace('html', 'html_tmp')
    if not os.path.exists(tmp_html):
        os.makedirs(tmp_html)

    print("Running postprocessing")
    post.postprocess(html, tmp_html)

    # replace old html with corrected stuff.
    if not args.debug:
        shutil.move(os.path.join(html, 'img'), tmp_html)
        shutil.rmtree(html)
        shutil.move(tmp_html, html)

    print('Begin updating xmls')
    if args.threads == 1:
        [update_xmls(x) for x in os.listdir(html) if x != 'img']
    else:
        results = [pool.apply_async(update_xmls, args=(x,)) for x in os.listdir(html) if x != 'img']
        [r.get() for r in results]
    print('End updating xmls')

    # Construct handles multiprocessing within it.

    construct(html, 'Figure Caption', 'Figure', os.path.join(args.output, 'figures.csv'), processes=args.threads)
    construct(html, 'Table Caption', 'Table', os.path.join(args.output, 'tables.csv'), processes=args.threads)

    # Parse html files to postgres db
    input_folder = ingestion_settings['input_folder']
    print(input_folder)

    # intermediate folder location (will be auto-generated)
    merge_folder = ingestion_settings['merge_folder']
    output_html = ingestion_settings['output_html']
    output_words = ingestion_settings['output_words']
    output_equations = ingestion_settings['output_equations']

    db_connect_str = ingestion_settings['db_connect_str']
    db_template_str = ingestion_settings['db_template_str']

    strip_tags = ingestion_settings['strip_tags']
    ignored_file_when_link = ingestion_settings['ignored_file_when_link']
    output_csv = os.path.join(args.output, "output.csv")
    corenlp_fd = '/app/stanford-corenlp-full-2018-10-05'

    if not args.noingest:
        conn = psycopg2.connect(db_template_str)
        conn.set_isolation_level(0)
        cur = conn.cursor()
        try:
            cur.execute("""DROP DATABASE cosmos""")
        except:
            print("Cannot drop db cosmos")
        try:
            cur.execute("""CREATE DATABASE cosmos""")
        except:
            print("Cannot create db cosmos")
        conn.close()
        parse_html_to_postgres(input_folder, output_html, merge_folder, output_words, output_equations, db_connect_str, strip_tags, ignored_file_when_link, output_csv, corenlp_fd)

    if args.keep_pages:
        if not os.path.exists(args.output + "/images/"):
            os.makedirs(args.output + "/images/")
        for page in glob.glob(f"{tmp}/images/*.png"):
            shutil.move(page, args.output + "/images/")

    #if not args.debug:
    #    shutil.rmtree(f'{tmp}')
