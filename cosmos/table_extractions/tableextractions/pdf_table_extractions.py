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
import glob
import base64
import sys
from typing import TypeVar, Callable

import click
from PyPDF2 import PdfFileReader
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from dask.distributed import Client, progress

import camelot
from utils import grouper

# Logging config
logging.basicConfig(
#                    filename = 'mylogs.log', filemode = 'w',
                    format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.ERROR)
logging.getLogger("camelot").setLevel(logging.ERROR)
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

    tempfile.tempdir = './'
    temp_file = tempfile.NamedTemporaryFile(mode="wb", suffix=".pdf", prefix=pdf_name, delete=False)
    # Create document
    try:
        for line in BytesIO(pdf_bytes):
            temp_file.write(line)
    except Exception:
        logging.info('Could not create pdf from bytes')
    finally:
        temp_file.close()
        return temp_file.name


#def run_table_extraction(get_metadata: Callable, insert_tables: Callable, n_jobs: int, skip: bool) -> None:
#    """
#    Entry point for extracting tables from ingested PDFs
#    """
#
#    logging.info('Running table extraction')
#    start_time = time.time()
#
#    client = MongoClient(os.environ["DBCONNECT"])
#    db = client.pdfs
#
#    buffer_size = n_jobs
#    tables_per_job = 10
#    total_tables = 0
#
#    for batch in get_metadata(db, buffer_size, tables_per_job):
#        t1 = time.time()
#        logs = Parallel(n_jobs=n_jobs)(delayed(table_extraction)(insert_tables, table_metadata, skip) for table_metadata in batch)
#
#        for log_list in logs:
#            for log in log_list:
#                logging.info(log)
#
#        t2 = time.time()
#        batch_size = sum([len(a) for a in batch])
#        batch_time = t2-t1
#        batch_rate = batch_size/(batch_time/60)
#
#        logging.info(f'Batch size: {batch_size} tables')
#        logging.info(f'Batch time: {batch_time} s')
#        logging.info(f'Batch extraction rate: {batch_rate} tables/min')
#
#        total_tables += batch_size
#
#    try:
#        shutil.rmtree(filedir_pdfs)
#    except OSError as e:
#        if e.errno != errno.ENOENT:
#            logging.info(f'Error: Could not delete tempfolder. {e}')
#
#    end_time = time.time()
#
#    total_time = end_time - start_time
#    total_rate = total_tables/(total_time/60)
#
#    logging.info(f'Completed table extractions')
#    logging.info(f'Number of tables processed: {total_tables} tables')
#    logging.info(f'Total time: {total_time} s')
#    logging.info(f'Table extraction rate: {total_rate} tables/min')
#    return
#
#
#def table_extraction(insert_tables: Callable, table_metadata: list, skip: bool) -> list:
#    """
#    Retrieve the tables from each table, and store them in mongodb.
#    """
#
#    client = MongoClient(os.environ["DBCONNECT"])
#    db = client.pdfs
#    coll_tables = db.tables
#
#    logs = []
#
#    try:
#        for table in table_metadata:
#            pdf_name = table['pdf_name']
#            table_page = table['page_num']
#            table_coords = table['coords']
#            table_coords2 = table['camelot_coords']
#
#            logs.append(f'Processing {pdf_name}, page {table_page}, coords {table_coords} and {table_coords2}')
#            # If document has already been scanned, ignore it based on skip settings
#            if skip & do_skip(coll_tables, pdf_name, table_page, table_coords):
#                logs.append('Document previously extracted. Skipping')
#            else:
#                # Extract the tables
#                df, flavor, extract_tables_logs = extract_tables(pdf_loc[pdf_name], table_coords2, table_page, "camelot_lattice_params.txt", "camelot_stream_params.txt")
#
#                prepare_table_data(table, df, flavor)
#
#                # Insert the data into mongodb
#                insert_tables_logs = insert_tables(coll_tables, table)
#
#                logs.extend(extract_tables_logs)
#                logs.extend(insert_tables_logs)
#    except Exception as e:
#        logs.append(f'An error occurred: {e}')
#    return logs
#
#
#def do_skip(coll_tables: pymongo.collection.Collection, raw_pdf_name: str, page_num: str, coords: str) -> bool:
#    """
#    Check if document is already scanned or not. If yes, skip it
#    """
#    return coll_tables.count_documents({'pdf_name': raw_pdf_name, 'page_num': page_num, 'coords': coords}, limit=1) != 0


def prepare_table_data(table: dict, df: bytes, flavor: str) -> list:
    '''
    Prepare data for mango extraction
    '''
    table['table_df'] = df
    table['flavor'] = flavor
    del table['camelot_coords']

    return table


def extract_tables(pdf_bytes: bytes, pdf_name: str, table_coords: list, table_page: str, lattice_params: str, stream_params: str, pkl=False) -> list:
    """
    Extract tables from pages.
    Needs:
        PDF bytes
        Table coordinates
        Page number within the PDF
    Improvements:
        Currently this gets called for each page of the PDF. It should be pretty easy to make it cover all pages in the PDF instead..
    """
    logs = []

    logs.append('Extracting tables')
    # TODO: dump pdf_bytes to a temp_file
    logging.info("Creating PDF")
    logging.info(pdf_name)
    logging.info(type(pdf_bytes))
    temp_pdf_name = create_pdf(pdf_name, pdf_bytes)
    logging.info("Created PDF")

# convert to camelot coords
    logging.info("Remapping coordinates")
    PDFfile = PdfFileReader(open(temp_pdf_name, 'rb'))
    if PDFfile.isEncrypted:
        PDFfile.decrypt('')
    PDFcoords = PDFfile.getPage(0).mediaBox

    pdf_width = PDFcoords[2]
    pdf_height = PDFcoords[3]

    # Taking out coords, and translating them for camelot
    coords = [int(coord) for coord in table_coords]

    x1 = int(coords[0])
    y1 = int(IMG_HEIGHT - coords[1])
    x2 = int(coords[2])
    y2 = int(IMG_HEIGHT - coords[3])
    coords_img = [x1, y1, x2, y2]

    pdf_img_ratio = pdf_height / IMG_HEIGHT
    coords_pdf = [float(pdf_img_ratio * x) for x in coords_img]

    coords_table = str(coords)[1:-1]
    coords_camelot = str(coords_pdf)[1:-1]


    stream_params = json.load(open(stream_params))
    logging.info(f"camelot.read_pdf({temp_pdf_name}, pages={table_page}, table_regions=[{coords_camelot}])")  
    tables = camelot.read_pdf(temp_pdf_name,
                                     pages=str(table_page),
                                     table_regions=[coords_camelot],
                                     **stream_params
                                     )

    if len(tables) == 0:
        logs.append('No tables found')
        return [None, None, logs, None, None]
    logs.append('Extracted table')
    table_df = tables[0].df
    acc = tables[0].parsing_report['accuracy']
    whitespace = tables[0].parsing_report['whitespace']
    flavor = "Stream"

    if pkl:
        table_df = pickle.dumps(table_df)
    os.remove(temp_pdf_name)
    import pdb; pdb.set_trace()
    # TODO: write Table object here


    return [table_df, flavor, logs, acc, whitespace]

def process_dataset(client):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    conn = engine.connect()
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    final_table_futures = []
    try:
        filenames = glob.glob("/input/*.pdf")
        for filename in filenames:
            # Get PDFs + read bytes
            pdf_bytes =  open(filename, 'rb').read()
            # TODO: poach byte reading + looping from ingest.
            pdf_name = os.path.basename(filename)
            logging.info(f"Working on {filename}")

            res = session.execute(
                    """
                    SELECT * FROM page_objects po
                        INNER JOIN (SELECT page_number, pdf_id, id FROM pages) AS p ON po.page_id = p.id
                        INNER JOIN (SELECT id, pdf_name FROM pdfs) AS d ON p.pdf_id = d.id
                      WHERE po.cls='Table' AND d.pdf_name=:pdf_name;'
                    """
                    , {'pdf_name' : pdf_name})

            for obj in res:
                logging.info("Object found!")
                pdf_name = obj['pdf_name']
                page_num = obj['page_number']
                coords = json.loads(obj['bounding_box'])

                logging.info(f"Extracting table from page {page_num} of {pdf_name}.")

                # TODO: this is a job per table.. passing lots of crap around when we should do it at the PDF level


                extract_tables(pdf_bytes, pdf_name, coords, page_num, "camelot_lattice_params.txt", "camelot_stream_params.txt")
                #final_table_futures.append(client.submit(extract_tables, pdf_bytes, pdf_name, coords, page_num, "camelot_lattice_params.txt", "camelot_stream_params.txt", resources={'extract_tables': 1}))
                progress(final_table_futures)
                client.gather(final_table_futures)
    except Exception as e:
        logging.error(str(e), exc_info=True)
        raise Exception(f'process_dataset error, {str(e)}')
    finally:
        session.close()
    return 


def run():
    client = Client('scheduler:8786')
    process_dataset(client)


if __name__ == '__main__':
    run()
