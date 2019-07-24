import click
import glob
import os
from joblib import Parallel, delayed
import re
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from pymongo import MongoClient
import pymongo


def code_ingestion(code_dir, db_insert_fn, num_processes, skip):
    """
    Ingest code
    """
    logging.info('Running code ingestion')
    logging.info('NOTE: Currently only Fortran files are supported for ingestion')
    # Obviously this needs to be expanded beyond fortran files, but this is proof of concept
    code_paths = glob.glob(os.path.join(code_dir, '*.f90')) 
    comment_reg = re.compile('.*!(.*)')
    comments = []
    for code in code_paths:
        with open(code, 'r') as rc:
            full_content = rc.read()
            for line_number, line in enumerate(rc):
                m = comment_reg.search(line)
                if m.group(1) is not None:
                    comment = m.group(1)
                    comments.append({'class': 'code', 'content': comment, 'line_number':line_number, 'full_content': full_content})
    db_insert_fn(comments)


def insert_code_mongo(comments):
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    objs = db.ocr_objs
    results = objs.insert_many(comments)
    logging.info(results)


@click.command()
@click.argument('code_dir')
@click.argument('num_processes')
def click_wrapper(code_dir, num_processes, skip):
    # TODO: Multiprocessing
    code_ingestion(code_dir, insert_code_mongo, int(num_processes))

if __name__ == '__main__':
    click_wrapper()
