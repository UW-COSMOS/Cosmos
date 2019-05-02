"""
Entry script for ingesting PDFs, creating the first object and passing it to the data store
"""

# Logging config
import logging
logging.basicConfig(format='%(levelname) :: %(asctime)s :: %(message)s', level=logging.DEBUG)

import click
import tempfile
import time
import subprocess
import os
import glob
from PIL import Image
from typing import Mapping, TypeVar
from pdf_extractor import parse_pdf
from pymongo import MongoClient


T = TypeVar('T')

@click.command()
@click.argument('pdf_path')
def run_pdf_ingestion(pdf_path: str):
    """
    Entry point for ingesting PDF documents
    """
    logging.info('Running proposal creation')
    start_time = time.time()
    # Make a tmp directory to let ghostscript write pngs
    with tempfile.TemporaryDirectory() as img_tmp:
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
            logging.info(out)
            logging.warn(err)
        pdf_obj = {}
        pdf_obj = load_page_data(img_tmp, pdf_obj)
        pdf_obj = load_pdf_metadata(pdf_path, pdf_obj)
        pdf_obj['event_stream'] = ['metadata', 'imagedata']
        # TODO: Set this according to docker instance
        client = MongoClient()
        pdf_collection = db.pdfs
        pdf_collection.insert_one(pdf_obj)


    end_time = time.time()
    logging.info(f'End running proposal creation. Total time: {end_time - start_time} s')



def load_page_data(img_dir: str, current_obj: Mapping[T, T]) -> Mapping[T, T]:
    """
    Iterate through the img directory, and retrieve the page level data
    """
    page_data = []
    for f in glob.glob(f'{img_tmp}/*'):
        page_obj = {}
        page_num = int(os.basename(f))
        img = Image.open(f)
        width, height = img.size
        with open(f, 'rb') as bimage:
            bstring = bimage.read()
            page_obj['bytes'] = bstring
        page_obj['page_width'] = width
        page_obj['page_height'] = height
        page_obj['page_num'] = page_num
        page_data.append(page_obj)
    current_obj['page_data'] = page_data
    return current_obj


def load_pdf_metadata(pdf_path: str, current_obj: Mapping[T, T]) -> Mapping[T, T]:
    """
    Load the pdf metadata
    """
    pdf_name = os.basename(pdf_path)
    df, limit = parse_pdf(pdf_path)
    df = df.to_dict()
    limit = list(limit)
    current_obj['metadata'] = df
    current_obj['metadata_dimension'] = limit
    current_obj['pdf_name'] = pdf_name
    return current_obj
    

if __name__ == '__main__':
    run_pdf_ingestion()

