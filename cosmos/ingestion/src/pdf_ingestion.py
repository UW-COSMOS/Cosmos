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
import os
import glob
import subprocess
import io
from preprocess import resize_png
from PIL import Image
from pdf_extractor import parse_pdf
from pymongo import MongoClient
import pymongo
from bson.objectid import ObjectId
import json

from joblib import Parallel, delayed


# No type hints on this one because we need to pickle it:
# https://github.com/python/typing/issues/511

def ingest_pdf(pdf_path, pdf_dir, db_insert_fn, db_insert_pages_fn, subprocess_fn, skip):
    with tempfile.TemporaryDirectory() as img_tmp:
        err = ""
        pdf_obj = {}
        logs = []
        pdf_obj, err = load_pdf_metadata(pdf_path, pdf_obj)
        if not err: 
            logs.append(err)
        pdf_name = pdf_obj['pdf_name']
        pdf_path = os.path.abspath("%s/%s" % (pdf_dir, pdf_name))
        # There needs to be hash index on pdf_name in the pdf and pages collections or else this will be so slow
        if skip and do_skip(pdf_path):
            return []
        pdf_obj["_id"] = ObjectId() # Want to know the _id _before_ insertion so we can tag pages in their collection
        subprocess_fn(pdf_path, img_tmp)
        pages = []
        pages = load_page_data(img_tmp, pdf_obj)
        try:
            pdf_logs = db_insert_fn(pdf_obj)
            pages_logs = db_insert_pages_fn(pages)
            logs.append(pdf_logs)
            logs.append(pages_logs)
        except pymongo.errors.DocumentTooLarge:
            return ['Document at {pdf_path} was too large, not inserted']
        except pymongo.errors.OperationFailure as e:
            # TODO: If this happens in pages, we should roll back the pdf insert.
            # We should really be using transactions here.
            return [f'Operation failure: {e}']
        return logs


def do_skip(pdf_name):
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    pdf_collection = db.raw_pdfs
    return pdf_collection.count_documents({'pdf_name': pdf_name}, limit=1) != 0


def run_pdf_ingestion(pdf_dir, db_insert_fn, db_insert_pages_fn, subprocess_fn, n_jobs, skip):
    """
    Entry point for ingesting PDF documents
    """

    logging.info('Running ingestion')
    start_time = time.time()

    pdf_paths = glob.glob(os.path.join(pdf_dir, '*.pdf'))
    logging.info(f'Ingesting {len(pdf_paths)} pdfs')
    logs = Parallel(n_jobs=n_jobs)(delayed(ingest_pdf)(pdf_path, pdf_dir, db_insert_fn, db_insert_pages_fn, subprocess_fn, skip) for pdf_path in pdf_paths)
    for log_list in logs:
        for log in log_list:
            logging.info(log)

    end_time = time.time()
    logging.info(f'End running metadata ingestion. Total time: {end_time - start_time} s')
    return


def run_ghostscript(pdf_path, img_tmp):
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


def insert_pages_mongo(pages):
    """
    Insert pdfs into mongodb
    """
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    page_collection = db.pages
    result = page_collection.insert_many(pages)
    return f'Inserted pages: {result}'


def insert_pdf_mongo(pdf):
    """
    Insert pdfs into mongodb
    """
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    pdf_collection = db.raw_pdfs
    try:
        result = pdf_collection.insert(pdf)
    except pymongo.errors.DocumentTooLarge:
        del pdf['bytes']
        try:
            result = pdf_collection.insert(pdf)
        except pymongo.errors.DocumentTooLarge as e:
            raise e
    return f"Inserted pdf : {result}"


def load_page_data(img_dir, current_obj):
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
            bytesio = io.BytesIO(bstring)
            img = resize_png(bytesio)
            # Convert it back to bytes
            resize_bytes_stream = io.BytesIO()
            img.save(resize_bytes_stream, format='PNG')
            resize_bytes = resize_bytes_stream.getvalue()
            page_obj['resize_bytes'] = resize_bytes
        page_obj['page_width'] = width
        page_obj['page_height'] = height
        page_obj['page_num'] = page_num
        page_obj['pdf_name'] = current_obj['pdf_name']
        page_obj['pdf_id'] = current_obj['_id']
        page_data.append(page_obj)
    return page_data


def load_pdf_metadata(pdf_path, current_obj):
    """
    Load the pdf metadata
    """
    pdf_name = os.path.basename(pdf_path)
    # Df maps coordinates to unicode, limit is the dimensions of the pdf
    df = limit = None
    err = ''
    try:
        df, limit = parse_pdf(pdf_path)
    except TypeError as e:
        err += f'{e}\n'
        df = limit = None
    except AttributeError as e:
        err += f'{e}\n'
        df = limit = None
    if df is not None:
        df = df.to_dict()
        # Hack here: throw this df into json and back
        df = json.dumps(df)
        df = json.loads(df)
        limit = list(limit)
    else:
        limit = None
    with open(pdf_path, 'rb') as rf:
        seq = rf.read()
        current_obj['bytes'] = seq
    current_obj['metadata'] = df
    current_obj['metadata_dimension'] = limit
    current_obj['pdf_name'] = pdf_name
    current_obj['event_stream'] = ['metadata']
    return current_obj, err


@click.command()
@click.argument('pdf_dir')
@click.argument('num_processes')
@click.option('--skip/--no-skip', help="Don't try to update already ingested pdfs. Good to use if you ran into an error on ingestion and want to continue the ingestion")
def click_wrapper(pdf_dir, num_processes, skip) -> None:
    run_pdf_ingestion(pdf_dir, insert_pdf_mongo, insert_pages_mongo, run_ghostscript, int(num_processes), skip)

if __name__ == '__main__':
    click_wrapper()

