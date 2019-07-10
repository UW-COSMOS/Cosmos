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
    table_extraction(skip)

    end_time = time.time()
    logging.info(f'End running table extractions. Total time: {end_time - start_time} s')
    return


def table_extraction(skip) -> None:
    """
    Retrieve the tables from each table, and store them in mongodb.
    """
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs

    coll_raw_pdfs = db.raw_pdfs
    coll_detect_pages = db.detect_pages
    coll_tables = db.tables

    docs_count = 0
    tables_count = 0

    tables_data = []

    # Get all the documents stored in raw_pdfs
    for raw_pdf in coll_raw_pdfs.find():
        raw_pdf_name = raw_pdf['pdf_name']
        raw_pdf_bytes = raw_pdf['bytes']

        logging.info(f'Scanning {raw_pdf_name}')
        #print(raw_pdf_name)
        #print()

        if skip & do_skip(coll_detect_pages, raw_pdf_name):
            logging.info(f'Document previously extracted. Skipping')
            continue

        # Get all page numbers and coordinates for each table in a document
        [pages, coords] = get_pages_and_coordinates(coll_detect_pages, raw_pdf_name)

        # Extract the tables
        [tables, flavor] = extract_tables(raw_pdf_bytes, pages, coords)

        # Prepare the data to be inserted
        tables_data_per_doc = load_tables(raw_pdf_name, pages, coords, tables, flavor)

        # Insert the data into mongodb
        tables_data.append(tables_data_per_doc)

        # Update the counts
        tables_count += len(tables)
        docs_count += 1
        #print()
        #print()

    # Insert the data into mongodb
    insert_tables_mongo(coll_tables, tables_data)

    logging.info(f'Extracted from  {docs_count} documents')
    logging.info(f'Tables extracted {tables_count}')

    print(docs_count)
    print(tables_count)


def do_skip(coll_detect_pages: pymongo.collection.Collection, raw_pdf_name: str):
    """
    Check if document is already scanned or not. If yes, skip it
    """
    return coll_detect_pages.count_documents({'pdf_name': raw_pdf_name}, limit=1) != 0


def get_pages_and_coordinates(coll_detect_pages: pymongo.collection.Collection, raw_pdf_name: str) -> list:
    """
    For each document in raw_pdfs, retrieve all pages that have detected tables, and their coordinates.
    """
    logging.info(f'Taking out documents for {raw_pdf_name}')

    pages = []
    coords = []

    for page in coll_detect_pages.find({"pdf_name": raw_pdf_name}):
        for detected_obj in page['detected_objs']:
            if detected_obj[1] == "Table":
                page_num = str(page["page_num"])
                table_coords = str(detected_obj[0])

                pages.append(page_num)
                coords.append(table_coords)

                logging.info(page_num, table_coords)
                #print(page_num, table_coords)

    return [pages, coords]


def extract_tables(raw_pdf_bytes:bytes, pages: list, coords: list) -> list:
    """
    Extract each table using both Lattice and Stream. Compare and choose the best one.
    """

    logging.info(f'Extracting tables')

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

        if tables_lattice.n == 0 and tables_stream.n == 0:
            table = tables_stream[0]
            flavor = "NA"
        elif tables_lattice.n == 0 or tables_lattice[0].accuracy < tables_stream[0].accuracy:
            table = tables_stream[0]
            flavor = "Stream"
        else:
            table = tables_lattice[0]
            flavor = "Lattice"

        df = table.df
        df_list.append(df)
        flavor_list.append(flavor)
        logging.info(f'Flavor {flavor}')

    os.remove('new.pdf')

    return [df_list, flavor_list]


def load_tables(pdf_name: str, pages: list, coords: list, tables: list, flavor: list) -> Mapping[T,T]:
    """
    Prepare the data containing the table, and corresponding doc, page, coords, and extraction flavor
    """
    logging.info(f'Storing document {pdf_name}')

    detected_tables = {}
    table_list = [pickle.dumps(table.df) for table in tables]

    detected_tables['pdf_name'] = pdf_name
    detected_tables['extracted_tables'] = list(zip(table_list, pages, coords, flavor))

    return detected_tables


def insert_tables_mongo(coll_tables: pymongo.collection.Collection, detected_tables: list[Mapping[T, T]]) -> None:
    """
    Insert tables into mongodb
    """
    logging.info(f'Inserting tables')
    coll_tables.insert_many(detected_tables)


@click.command()
@click.argument('num_processes')
@click.option('--skip/--no-skip', help="Don't try to update already ingested pdfs. Good to use if you "
                                                       "ran into an error on ingestion and want to continue the "
                                                       "ingestion")
def click_wrapper(num_processes: str, skip) -> None:
    print(skip)
    run_table_extraction(int(num_processes), skip)


if __name__ == '__main__':
    click_wrapper()
