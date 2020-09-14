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
from random import shuffle
import sys
from typing import TypeVar, Callable

import click
from PyPDF2 import PdfFileReader
from sqlalchemy import create_engine
from sqlalchemy.sql import text
from sqlalchemy.orm import sessionmaker
from dask.distributed import Client, progress, fire_and_forget
from .schema import Pdf, Page, PageObject, Table

import camelot
from .utils import grouper

# Logging config
logging.basicConfig(
#                    filename = 'mylogs.log', filemode = 'w',
                    format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.INFO)
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

def prepare_table_data(table: dict, df: bytes, flavor: str) -> list:
    '''
    Prepare data for mango extraction
    '''
    table['table_df'] = df
    table['flavor'] = flavor
    del table['camelot_coords']

    return table

def extract_tables(pdf_bytes: bytes, pdf_name: str, table_coords: list, table_page: str, stream_params: str, object_id: int, page_id: int, pkl=True) -> list:
    """
    Extract tables from pages.
    Needs:
        PDF bytes
        Table coordinates
        Page number within the PDF
    Improvements:
        Currently this gets called for each page of the PDF. It should be pretty easy to make it cover all pages in the PDF instead..
    """
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    tsession = Session()

    logs = []

    temp_pdf_name = create_pdf(pdf_name, pdf_bytes)

    # convert to camelot coords
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

#    stream_params = json.load(open(stream_params))
    try:
        tables = camelot.read_pdf(temp_pdf_name,
                                         pages=str(table_page),
                                         table_regions=[coords_camelot],
                                         **stream_params
                                         )
    except:
        logs.append('Issue extracting tables')
        return [None, None, logs, None, None]
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
    table = Table(page_object_id=object_id, page_id=page_id, df=table_df)
    tsession.add(table)
    tsession.commit()
    tsession.close()

def process_dataset(client):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
    conn = engine.connect()
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    final_table_futures = []
    try:
        filenames = glob.glob("/input/*.pdf")
        shuffle(filenames)
        for filename in filenames:
            # Get PDFs + read bytes
            pdf_bytes =  open(filename, 'rb').read()
            # TODO: poach byte reading + looping from ingest.
            pdf_name = os.path.basename(filename)
            logging.info(f"Working on {filename}")
            q = text("SELECT *, po.id as object_id FROM page_objects po INNER JOIN (SELECT page_number, pdf_id, id FROM pages) AS p ON po.page_id = p.id INNER JOIN (SELECT id, pdf_name FROM pdfs) AS d ON p.pdf_id = d.id WHERE po.cls='Table' AND d.pdf_name=:pdf_name AND po.page_id NOT IN (SELECT DISTINCT(page_id) from tables)")
            res = conn.execute(q, pdf_name=pdf_name)
            for obj in res:
                pdf_name = obj['pdf_name']
                page_num = obj['page_number']
                coords = json.loads(obj['bounding_box'])
                object_id = obj['object_id']
                page_id = obj['page_id']
                r1 = extract_tables(pdf_bytes, pdf_name, coords, page_num, json.load(open("tableextractions/camelot_stream_params.txt")), object_id, page_id)
#                r1 = client.submit(extract_tables, pdf_bytes, pdf_name, coords, page_num, json.load(open("tableextractions/camelot_stream_params.txt")), object_id, page_id, resources={'extract_tables' : 1})
#                fire_and_forget(r1)
                #final_table_futures.append(client.submit(extract_tables, pdf_bytes, pdf_name, coords, page_num, "camelot_lattice_params.txt", "camelot_stream_params.txt", resources={'extract_tables': 1}))
#                progress(final_table_futures)
#                client.gather(final_table_futures)
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
