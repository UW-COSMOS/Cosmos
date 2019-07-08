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
from pdf_extractor import parse_pdf
from pymongo import MongoClient
import json
import camelot
import pickle

from io import BytesIO


T = TypeVar('T')

def run_pdf_ingestion(pdf_dir: str, db_insert_fn: Callable[[Mapping[T, T]], None], subprocess_fn: Callable[[str, str], None]) -> Mapping[T, T]:
    """
    Entry point for ingesting PDF documents
    """
    logging.info('Running ingestion')
    start_time = time.time()
    pdfs = []
    for pdf_path in glob.glob(os.path.join(pdf_dir, '*.pdf')):
        with tempfile.TemporaryDirectory() as img_tmp:
            subprocess_fn(pdf_path, img_tmp)
            pdf_obj = {}
            pdf_obj = load_page_data(img_tmp, pdf_obj)
            pdf_obj = load_pdf_metadata(pdf_path, pdf_obj)
            pdf_obj = load_pdf_tables(pdf_path, pdf_obj)
            pdfs.append(pdf_obj)

    if len(pdfs) > 0:
        db_insert_fn(pdfs)
    else:
        logging.info("The pdfs directory "+ pdf_dir + " was empty. Nothing inserted")

    end_time = time.time()
    logging.info(f'End running ingestion. Total time: {end_time - start_time} s')
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


def insert_pdfs_mongo(pdfs: Mapping[T, T]) -> None:
    """
    Insert pdfs into mongodb
    """
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    pdf_collection = db.raw_pdfs
    result = pdf_collection.insert_many(pdfs)


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
        page_data.append(page_obj)
    current_obj['page_data'] = page_data
    current_obj['event_stream'] = ['imagedata']
    return current_obj


def load_pdf_metadata(pdf_path: str, current_obj: Mapping[T, T]) -> Mapping[T, T]:
    """
    Load the pdf metadata
    """
    pdf_name = os.path.basename(pdf_path)
    # Df maps coordinates to unicode, limit is the dimensions of the pdf
    df, limit = parse_pdf(pdf_path)
    df = df.to_dict()
    # Hack here: throw this df into json and back
    df = json.dumps(df)
    df = json.loads(df)
    limit = list(limit)
    current_obj['metadata'] = df
    current_obj['metadata_dimension'] = limit
    current_obj['pdf_name'] = pdf_name
    current_obj['event_stream'].append('metadata')
    return current_obj

def load_pdf_tables(pdf_path: str, current_obj: Mapping[T,T]) -> Mapping[T, T]:
    tables = camelot.read_pdf(pdf_path)
    table_list = [pickle.dumps(table.df) for table in tables]
    current_obj['extracted_tables'] = table_list
    return current_obj

def detect_tables_from_pages():
    print("Starting")
    start_time = time.time()
    
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    coll_raw_pdfs = db.raw_pdfs
    coll_detect_pages = db.detect_pages
    docs = []
    df_list = []
    
    # Get all the documents stored in raw_pdfs
    for raw_pdf in coll_raw_pdfs.find():
        pages = []
        coords = []

        raw_pdf_name = raw_pdf['pdf_name']
        raw_pdf_bytes = raw_pdf['bytes']
        print(raw_pdf_name)

        # Get all corresponding pages with tables and their coordinates from detect_pages
        for page in coll_detect_pages.find({"pdf_name":raw_pdf['pdf_name']}):
            for detected_obj in page['detected_objs']:
                if detected_obj[1] == "Table":
                    pdf_name = str(page["pdf_name"])
                    page_num = str(page["page_num"])
                    table_coords = str(detected_obj[0])

                    pages.append(page_num)
                    coords.append(table_coords)

                    print(pdf_name, page_num, table_coords)

        print()
        
        mapped = list(zip(pages, coords))

        # For all new documents, create a new file
        file = open('new.pdf', 'wb')
        for line in BytesIO(raw_pdf_bytes):
            file.write(line)
        file.close()

        # For each document, find all tables at specified coords
        # Compare each table extraction with Lattice and Stream, and choose best one
        for pair in mapped:
            my_page = pair[0]
            my_coords = pair[1][1:-1].replace('.0', '').replace(' ', '')

            tables_stream = camelot.read_pdf('new.pdf',
                                             pages = my_page,
                                             flavor = 'stream',
                                             table_region = [my_coords],
                                             flag_size = True,
                                             strip_text = ' .\n',
                                             edge_tol = 25
                                            )
    
            tables_lattice = camelot.read_pdf('new.pdf',
                                              pages = my_page, 
                                              flavor = 'lattice',
                                              table_region = [my_coords],
                                              split_text = True,
                                              flag_size = True,
                                              strip_text = ' .\n',
                                              line_scale = 100,
                                              shift_text = [''],
                                              copy_text = ['h', 'v']
                                             )
            
            if tables_lattice.n == 0 or tables_lattice[0].accuracy < tables_stream[0].accuracy:
                table = tables_stream[0]
                flavor = "Stream"
            else:
                table = tables_lattice[0]
                flavor = "Lattice"

            df = table.df
            df_list.append(df)
        
        os.remove('new.pdf')
        docs.append(raw_pdf_name)
        print()
        print()
    
    print(len(docs))
    print(len(df_list))
    
    end_time = time.time()
    total_time = end_time - start_time
    print("Total time taken is: " + total_time)

@click.command()
@click.argument('pdf_dir')
def click_wrapper(pdf_dir: str) -> None:   
    #run_pdf_ingestion(pdf_dir, insert_pdfs_mongo, run_ghostscript)
    detect_tables_from_pages()

if __name__ == '__main__':
    click_wrapper()

