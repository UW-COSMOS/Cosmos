"""
Script for extracting tables from PDFs, given location (page and coords), and storing them in mongodb
"""

import errno
import json
import logging
import os
import pickle
import shutil
import tempfile
import time
from io import BytesIO
from os import path
import sys
from typing import TypeVar, Callable

import click
import pymongo
from PyPDF2 import PdfFileReader
from joblib import Parallel, delayed
from pymongo import MongoClient

import camelot
from tableextractions.utils import grouper

# Logging config
logging.basicConfig(
#                    filename = 'mylogs.log', filemode = 'w',
                    format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
T = TypeVar('T')

IMG_HEIGHT = IMG_WIDTH = 1920
pdf_loc = {}


def create_pdf(pdf_name: str, pdf_bytes: bytes) -> None:
    """
    Create PDF and store it in filedir_pdf
    """
    # If path exists, do not create pdf

    if pdf_name in pdf_loc:
        return

    tempfile.tempdir = filedir_pdfs
    temp_file = tempfile.NamedTemporaryFile(mode="wb", suffix=".pdf", prefix=pdf_name[:-4]+'_', delete=False)

    # Create document
    try:
        for line in BytesIO(pdf_bytes):
            temp_file.write(line)
    except Exception:
        logging.info('Could not create pdf from bytes')
    finally:
        temp_file.close()
        pdf_loc[pdf_name] = temp_file.name


def run_table_extraction(get_metadata: Callable, insert_tables: Callable, n_jobs: int, skip: bool) -> None:
    """
    Entry point for extracting tables from ingested PDFs
    """

    logging.info('Running table extraction')
    start_time = time.time()

    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs

    buffer_size = n_jobs
    tables_per_job = 10
    total_tables = 0

    for batch in get_metadata(db, buffer_size, tables_per_job):
        t1 = time.time()
        logs = Parallel(n_jobs=n_jobs)(delayed(table_extraction)(insert_tables, table_metadata, skip) for table_metadata in batch)

        for log_list in logs:
            for log in log_list:
                logging.info(log)

        t2 = time.time()
        batch_size = sum([len(a) for a in batch])
        batch_time = t2-t1
        batch_rate = batch_size/(batch_time/60)
        
        logging.info(f'Batch size: {batch_size} tables')
        logging.info(f'Batch time: {batch_time} s')
        logging.info(f'Batch extraction rate: {batch_rate} tables/min')

        total_tables += batch_size
    
    try:
        shutil.rmtree(filedir_pdfs)
    except OSError as e:
        if e.errno != errno.ENOENT:
            logging.info(f'Error: Could not delete tempfolder. {e}')

    end_time = time.time()

    total_time = end_time - start_time
    total_rate = total_tables/(total_time/60)

    logging.info(f'Completed table extractions')
    logging.info(f'Number of tables processed: {total_tables} tables')
    logging.info(f'Total time: {total_time} s')
    logging.info(f'Table extraction rate: {total_rate} tables/min')
    return


def table_extraction(insert_tables: Callable, table_metadata: list, skip: bool) -> list:
    """
    Retrieve the tables from each table, and store them in mongodb.
    """

    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    coll_tables = db.tables

    logs = []

    try:
        for table in table_metadata:
            pdf_name = table['pdf_name']
            table_page = table['page_num']
            table_coords = table['coords']
            table_coords2 = table['camelot_coords']

            logs.append(f'Processing {pdf_name}, page {table_page}, coords {table_coords} and {table_coords2}')
            # If document has already been scanned, ignore it based on skip settings
            if skip & do_skip(coll_tables, pdf_name, table_page, table_coords):
                logs.append('Document previously extracted. Skipping')
            else:
                # Extract the tables
                df, flavor, extract_tables_logs = extract_tables(pdf_loc[pdf_name], table_coords2, table_page, "camelot_lattice_params.txt", "camelot_stream_params.txt")

                prepare_table_data(table, df, flavor)

                # Insert the data into mongodb
                insert_tables_logs = insert_tables(coll_tables, table)

                logs.extend(extract_tables_logs)
                logs.extend(insert_tables_logs)
    except Exception as e:
        logs.append(f'An error occurred: {e}')
    return logs


def do_skip(coll_tables: pymongo.collection.Collection, raw_pdf_name: str, page_num: str, coords: str) -> bool:
    """
    Check if document is already scanned or not. If yes, skip it
    """
    return coll_tables.count_documents({'pdf_name': raw_pdf_name, 'page_num': page_num, 'coords': coords}, limit=1) != 0


def prepare_table_data(table: dict, df: bytes, flavor: str) -> list:
    '''
    Prepare data for mango extraction
    '''
    table['table_df'] = df
    table['flavor'] = flavor
    del table['camelot_coords']

    return table


def extract_tables(pdf_name: str, table_coords: str, table_page: str, lattice_params: str, stream_params: str) -> list:
    """
    Extract each table using both Lattice and Stream. Compare and choose the best one.
    """
    logs = []

    logs.append('Extracting tables')

    try:
        stream_params = json.load(open(stream_params))
        tables_stream = camelot.read_pdf(pdf_name,
                                         pages=table_page,
                                         table_areas=[table_coords],
                                         **stream_params
                                         )

        lattice_params = json.load(open(lattice_params))
        tables_lattice = camelot.read_pdf(pdf_name,
                                          pages=table_page,
                                          table_areas=[table_coords],
                                          **lattice_params
                                          )

        if tables_lattice.n == 0 and tables_stream.n == 0:
            raise Exception('Table not detected')

        elif tables_lattice.n == 0 or tables_lattice[0].accuracy < tables_stream[0].accuracy:
            logs.append('Extracted table')
            table_df = tables_stream[0].df
            flavor = "Stream"

        else:
            logs.append('Extracted table')
            table_df = tables_lattice[0].df
            flavor = "Lattice"

    except Exception as e:
        logs.append(f'An error occurred: {e}')
        table_df = None
        flavor = "NA"

    pkld_df = pickle.dumps(table_df)

    return [pkld_df, flavor, logs]


def load_table_metadata(db: pymongo.database.Database, buffer_size: int = 50, tables_per_job: int = 10) -> list:
    """
    Load documents buffer_size at a time
    """
    table_data = []
    pdf_data = {}

    coll_raw_pdfs = db.raw_pdfs
    coll_detect_pages = db.postprocess_pages
    mongo_query_detect_pages = coll_detect_pages.find({},
                                                      {'pdf_name': 1, 'page_num': 1, 'pp_detected_objs': 1},
                                                      no_cursor_timeout=True)

    global filedir_pdfs

    # Go through detect pages and get each page of a document
    with tempfile.TemporaryDirectory() as filedir_pdfs:
        for page in mongo_query_detect_pages:
            pdf_name = page['pdf_name']

            # Get bytes of page's pdf
            if pdf_name in pdf_data:
                pdf_bytes = pdf_data[pdf_name]
            else:
                mongo_query_raw_pdfs = coll_raw_pdfs.find({'pdf_name': pdf_name, 'bytes': {'$exists': True}},
                                                          {'bytes': 1},
                                                          no_cursor_timeout=True)
                if not coll_raw_pdfs.count_documents({'pdf_name': pdf_name, 'bytes': {'$exists': True}},
                                                     limit=1):
                    continue

                for raw_pdf in mongo_query_raw_pdfs.limit(1):
                    pdf_bytes = raw_pdf['bytes']
                    pdf_data[pdf_name] = pdf_bytes

            for detected_obj in page['pp_detected_objs']:
                # Get table coords and page of table in pdf
                if detected_obj[1] == "Table":
                    page_num = str(page['page_num'])

                    create_pdf(pdf_name, pdf_bytes)
                    
                    PDFfile = PdfFileReader(open(pdf_loc[pdf_name], 'rb'))
                    if PDFfile.isEncrypted:
                        PDFfile.decrypt('')
                    PDFcoords = PDFfile.getPage(0).mediaBox
                    
                    pdf_width = PDFcoords[2]
                    pdf_height = PDFcoords[3]

                    # Taking out coords, and translating them for camelot
                    detected_coords = detected_obj[0]
                    coords = [int(coord) for coord in detected_coords]

                    x1 = int(coords[0])
                    y1 = int(IMG_HEIGHT - coords[1])
                    x2 = int(coords[2])
                    y2 = int(IMG_HEIGHT - coords[3])
                    coords_img = [x1, y1, x2, y2]

                    pdf_img_ratio = pdf_height / IMG_HEIGHT
                    coords_pdf = [float(pdf_img_ratio * x) for x in coords_img]

                    coords_table = str(coords)[1:-1]
                    coords_camelot = str(coords_pdf)[1:-1]

                    page_data = {"pdf_name": pdf_name, "bytes": pdf_bytes,
                                 "coords": coords_table, "camelot_coords": coords_camelot,
                                 "page_num": page_num}
                    table_data.append(page_data)

                # Return grouped list once enough tables have been identified
                if len(table_data) == buffer_size * tables_per_job:
                    yield grouper(table_data, tables_per_job)

                    # Start fresh after a yield
                    table_data = []

        yield grouper(table_data, tables_per_job)


def insert_tables_mongo(coll_tables: pymongo.collection.Collection, detected_table: dict) -> list:
    """
    Insert tables into mongodb
    """
    result = coll_tables.insert_one(detected_table)
    return [f'Inserted table: {result}']


@click.command()
@click.argument('num_processes')
@click.option('--skip/--no-skip', help="Don't try to update already extracted pdfs. Good to use if you ran into an "
                                       "error on ingestion and want to continue the ingestion")
def click_wrapper(num_processes: str, skip) -> None:
    # print(f"Pickle table_extraction: {is_picklable(table_extraction)}")
    print(skip)
    run_table_extraction(load_table_metadata, insert_tables_mongo, int(num_processes), skip)


if __name__ == '__main__':
    click_wrapper()


