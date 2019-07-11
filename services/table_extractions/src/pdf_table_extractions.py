"""
Script for extracting tables from PDFs, given location (page and coords), and storing them in mongodb
"""

# Logging config
import logging

logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
import click
import time
import os
from typing import Mapping, TypeVar
import pymongo
from pymongo import MongoClient
import camelot
import pickle
from joblib import Parallel, delayed
from io import BytesIO


T = TypeVar('T')


def run_table_extraction(n_jobs, skip) -> None:
    """
    Entry point for extracting tables from ingested PDFs
    """

    logging.info('Running table extraction')
    start_time = time.time()

    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs

    for batch in load_raw_pdfs(db, 100):
        logs = Parallel(n_jobs=n_jobs)(delayed(table_extraction)(raw_pdf, db, skip) for raw_pdf in batch)

        for log_list in logs:
            for log in log_list:
                logging.info(log)

    end_time = time.time()
    logging.info(f'End running table extractions. Total time: {end_time - start_time} s')
    return


def load_raw_pdfs(db, buffer_size) -> list:
    """
    Load documents buffer_size at a time
    """
    coll_raw_pdfs = db.raw_pdfs
    current_docs = []

    for doc in coll_raw_pdfs.find():
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs


def table_extraction(raw_pdf, db, skip) -> list:
    """
    Retrieve the tables from each table, and store them in mongodb.
    """

    coll_detect_pages = db.detect_pages
    coll_tables = db.tables

    logs = []

    raw_pdf_name = raw_pdf['pdf_name']
    raw_pdf_bytes = raw_pdf['bytes']

    logs.append(f'Scanning {raw_pdf_name}')

    # If document has already been scanned, ignore it based on skip settings
    if skip & do_skip(coll_tables, raw_pdf_name):
        logs.append('Document previously extracted. Skipping')
    else:
        # Get all page numbers and coordinates for each table in a document
        [pages, coords, pages_and_coords_logs] = get_pages_and_coordinates(coll_detect_pages, raw_pdf_name)

        # If no pages with tables are found, skip
        if not pages:
            logs.append('No tables detected in this document')
        else:
            # Extract the tables
            [table_df, flavor, extract_tables_logs] = extract_tables(raw_pdf_bytes, pages, coords)

            # Prepare the data to be inserted
            tables_data = load_tables(raw_pdf_name, pages, coords, table_df, flavor)

            # Insert the data into mongodb
            insert_tables_mongo_logs = insert_tables_mongo(coll_tables, tables_data)

            logs.append(pages_and_coords_logs)
            logs.append(extract_tables_logs)
            logs.append(insert_tables_mongo_logs)

    return logs


def do_skip(coll_tables: pymongo.collection.Collection, raw_pdf_name: str):
    """
    Check if document is already scanned or not. If yes, skip it
    """
    return coll_tables.count_documents({'pdf_name': raw_pdf_name}, limit=1) != 0


def get_pages_and_coordinates(coll_detect_pages: pymongo.collection.Collection, raw_pdf_name: str) -> list:
    """
    For each document in raw_pdfs, retrieve all pages that have detected tables, and their coordinates.
    """
    logs = []
    pages = []
    coords = []

    logs.append(f'Taking out metadata for {raw_pdf_name}')

    for page in coll_detect_pages.find({"pdf_name": raw_pdf_name}):
        for detected_obj in page['detected_objs']:
            if detected_obj[1] == "Table":
                page_num = str(page["page_num"])
                table_coords = str(detected_obj[0])

                pages.append(page_num)
                coords.append(table_coords)

                logs.append(f'Page: {page_num}, Coords: {table_coords}')

    return [pages, coords, logs]


def extract_tables(raw_pdf_bytes:bytes, pages: list, coords: list) -> list:
    """
    Extract each table using both Lattice and Stream. Compare and choose the best one.
    """
    logs = []

    logs.append('Extracting tables')

    file = open('new.pdf', 'wb')
    for line in BytesIO(raw_pdf_bytes):
        file.write(line)
    file.close()

    pages_and_coords = list(zip(pages, coords))
    df_list = []
    flavor_list = []

    for pair in pages_and_coords:
        my_page = pair[0]
        my_coords = pair[1][1:-1].replace('.0', '').replace(' ', '')

        try:
            tables_stream = camelot.read_pdf('new.pdf',
                                         pages=my_page,
                                         flavor='stream',
                                         table_region=[my_coords],
                                         flag_size=True,
                                         strip_text=' .\n',
                                         edge_tol=25
                                         )

            tables_lattice = camelot.read_pdf('new.pdf',
                                          pages=my_page,
                                          flavor='lattice',
                                          table_region=[my_coords],
                                          split_text=True,
                                          flag_size=True,
                                          strip_text=' .\n',
                                          line_scale=100,
                                          shift_text=[''],
                                          copy_text=['h', 'v']
                                          )

            if tables_lattice.n == 0 or tables_lattice[0].accuracy < tables_stream[0].accuracy:
                table = tables_stream[0]
                flavor = "Stream"
            else:
                table = tables_lattice[0]
                flavor = "Lattice"

            df = table.df
        
        except:
            logging.info('Error: Table not detected')
            df = None
            flavor = "NA"

        df_list.append(df)
        flavor_list.append(flavor)
        logging.info(f'Flavor {flavor}')

    os.remove('new.pdf')

    return [df_list, flavor_list, logs]


def load_tables(pdf_name: str, pages: list, coords: list, table_df: list, flavor: list) -> list:
    """
    Prepare the data containing the table, and corresponding doc, page, coords, and extraction flavor
    """
    detected_tables = {}

    table_list = [pickle.dumps(table) for table in table_df]

    detected_tables['pdf_name'] = pdf_name
    detected_tables['extracted_tables'] = list(zip(table_list, pages, coords, flavor))

    return [detected_tables]


def insert_tables_mongo(coll_tables: pymongo.collection.Collection, detected_tables: list) -> str:
    """
    Insert tables into mongodb
    """
    result = coll_tables.insert_one(detected_tables)
    return f'Inserted tables: {result}'


@click.command()
@click.argument('num_processes')
@click.option('--skip/--no-skip', help="Don't try to update already extracted pdfs. Good to use if you ran into an "
                                       "error on ingestion and want to continue the ingestion")
def click_wrapper(num_processes: str, skip) -> None:
    print(skip)
    run_table_extraction(int(num_processes), skip)


if __name__ == '__main__':
    click_wrapper()
