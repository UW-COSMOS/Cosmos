"""
Auxiliary script for taking out tables from extracted objects
"""

import os
import tempfile
from io import BytesIO
import pickle

from PyPDF2 import PdfFileReader
from pymongo import MongoClient
from tableextractions.pdf_table_extractions import extract_tables

IMG_HEIGHT = 1920


def extract_bytes(pdf_name: str) -> bytes:
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    coll_raw_pdfs = db.raw_pdfs
    pdf_bytes = None

    mongo_query_raw_pdfs = coll_raw_pdfs.find({'pdf_name': pdf_name, 'bytes': {'$exists': True}}, {'bytes': 1}, no_cursor_timeout=True)
    if coll_raw_pdfs.count_documents({'pdf_name': pdf_name, 'bytes': {'$exists': True}}, limit=1):
        for raw_pdf in mongo_query_raw_pdfs.limit(1):
            pdf_bytes = raw_pdf['bytes']

    return pdf_bytes


def create_pdf(pdf_name: str, pdf_bytes: bytes) -> str:
    """
    Create PDF and store it in filedir_pdf
    """

    temp_file = tempfile.NamedTemporaryFile(mode="wb", suffix=".pdf", prefix=pdf_name[:-4]+'_', delete=False)

    # Create document
    try:
        for line in BytesIO(pdf_bytes):
            temp_file.write(line)
    except Exception:
        logging.info('Could not create pdf from bytes')
    finally:
        temp_file.close()

    return temp_file


def convert_coords(pdf_name: str, detected_obj: list):
    PDFfile = PdfFileReader(open(pdf_name, 'rb'))
    if PDFfile.isEncrypted:
        PDFfile.decrypt('')
    PDFcoords = PDFfile.getPage(0).mediaBox

    pdf_width = PDFcoords[2]
    pdf_height = PDFcoords[3]

    # Taking out coords, and translating them for camelot
    detected_coords = detected_obj
    coords = [int(coord) for coord in detected_coords]

    x1 = int(coords[0])
    y1 = int(IMG_HEIGHT - coords[1])
    x2 = int(coords[2])
    y2 = int(IMG_HEIGHT - coords[3])
    coords_img = [x1, y1, x2, y2]

    pdf_img_ratio = pdf_height / IMG_HEIGHT
    coords_pdf = [float(pdf_img_ratio * x) for x in coords_img]

    coords_camelot = str(coords_pdf)[1:-1]

    return coords_camelot


#def extract_tables(pdf_name: str, table_coords: str, table_page: str) -> list:
#    """
#    Extract each table using both Lattice and Stream. Compare and choose the best one.
#    """
#    try:
#        stream_params = json.load(open("camelot_stream_params.txt"))
#        tables_stream = camelot.read_pdf(pdf_name,
#                                         pages=table_page,
#                                         table_areas=[table_coords],
#                                         **stream_params
#                                         )
#
#        lattice_params = json.load(open("camelot_stream_params.txt"))
#        tables_lattice = camelot.read_pdf(pdf_name,
#                                          pages=table_page,
#                                          table_areas=[table_coords],
#                                          **lattice_params
#                                          )
#
#        if tables_lattice.n == 0 and tables_stream.n == 0:
#            raise Exception('Table not detected')
#
#        elif tables_lattice.n == 0 or tables_lattice[0].accuracy < tables_stream[0].accuracy:
#            table_df = tables_stream[0].df
#            flavor = "Stream"
#
#        else:
#            table_df = tables_lattice[0].df
#            flavor = "Lattice"
#
#    except Exception as e:
#        table_df = None
#        flavor = "NA"
#
#    pkld_df = pickle.dumps(table_df)

#    return pkld_df


def extract_table_from_obj(pdf_name: str, page_num: str, coords: list):
    pdf_bytes = extract_bytes(pdf_name)
    table_df = None

    if pdf_bytes is not None:
        temp_file = create_pdf(pdf_name, pdf_bytes)
        file_loc = temp_file.name
        coords_camelot = convert_coords(file_loc, coords)
        table_df = extract_tables(file_loc, coords_camelot, page_num)[0]
        os.remove(temp_file.name)

    return table_df
