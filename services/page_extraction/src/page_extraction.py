"""
Entry script for ingesting PDFs, creating the first object and passing it to the data store
"""

# Logging config
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
import click
import tempfile
import time
import subprocess
import os
import glob
from PIL import Image
from typing import Mapping, TypeVar, Callable
from pymongo import MongoClient
import json


T = TypeVar('T')


def run_page_extraction(pdf_dir: str, db_insert_fn: Callable[[Mapping[T, T]], None], subprocess_fn: Callable[[str, str], None]) -> Mapping[T, T]:
    """
    Entry point for ingesting PDF documents
    """
    logging.info('Running ingestion')
    pdfs = []
    logging.info('Starting page extraction watch process')
    start_time = time.time()
    client = MongoClient(os.environ["DBCONNECT"])
    logging.info(f'Connected to client: {client}. Setting watch on raw_pdfs collection')
    db = client.pdfs
    with db.raw_pdfs.watch([{'$match': {'operationType': 'insert'}}]) as stream:
        for doc in stream:
            doc = doc["fullDocument"]
            with tempfile.TemporaryDirectory() as img_tmp:
                print(doc.keys())
                pdf_path = os.path.abspath("%s/%s" % (pdf_dir, doc["pdf_name"]))
                # TODO: get real pdf_path as pdf_dir + pdf_name (from doc)
                subprocess_fn(pdf_path, img_tmp)
                pages = []
                pages = load_page_data(img_tmp, doc)
                db_insert_fn(pages)

    end_time = time.time()
    logging.info(f'Finishing page extraction. Total time: {end_time - start_time} s')
    return pdfs

def run_ghostscript(pdf_path: str, img_tmp: str) -> None:
    """
    Run ghostscript as a subprocess over pdf files
    """
    with tempfile.TemporaryFile() as gs_stdout, tempfile.TemporaryFile() as gs_stderr:
        subprocess.run(['gs', '-dBATCH',
                              '-dNOPAUSE',
                              '-sDEVICE=png16m',
                              '-dGraphicsAlphaBits=4',
                              '-dTextAlphaBits=4',
                              '-r600',
                              f'-sOutputFile="{img_tmp}/%d"',
                              pdf_path
                        ], stdout=gs_stdout, stderr=gs_stderr)
        out = gs_stdout.read()
        err = gs_stdout.read()
        if len(out):
            logging.info("Ghostscript output:")
            logging.info(str(out))
        if len(err):
            logging.warning("Ghostscript err: ")
            logging.warning(err)


def insert_pages_mongo(pages: Mapping[T, T]) -> None:
    """
    Insert pdfs into mongodb
    TODO: Pass correct config
    """
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    page_collection = db.pages
    result = page_collection.insert_many(pages)
    # TODO: page_data should ideally get stored in a page collection with a link up to these docid (?)


def load_page_data(img_dir: str, current_obj: Mapping[T, T]) -> Mapping[T, T]:
    """
    Iterate through the img directory, and retrieve the page level data
    """
    page_data = []
    for f in glob.glob(f'{img_dir}/*'):
        logging.info(f)
        page_obj = {}
        page_num = int(os.path.basename(f))
        img = Image.open(f)
        width, height = img.size
        with open(f, 'rb') as bimage:
            bstring = bimage.read()
            page_obj['bytes'] = bstring
        page_obj['page_width'] = width
        page_obj['page_height'] = height
        page_obj['page_num'] = page_num
        page_obj['pdf_name'] = current_obj['pdf_name']
        page_obj['pdf_id'] = current_obj['_id']
        page_data.append(page_obj)
    return page_data

@click.command()
@click.argument('pdf_dir')
def click_wrapper(pdf_dir: str) -> None:
    run_page_extraction(pdf_dir, insert_pages_mongo, run_ghostscript)

if __name__ == '__main__':
    click_wrapper()

