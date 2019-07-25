"""
Script for extracting tables from PDFs, given location (page and coords), and storing them in mongodb
"""

# Logging config
import logging
import click
import time
import os
from os import path
from typing import Mapping, TypeVar
import pymongo
from pymongo import MongoClient
import camelot
import pickle
from joblib import Parallel, delayed
from io import BytesIO
from itertools import zip_longest

logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
T = TypeVar('T')


def run_table_extraction(n_jobs: int, skip: bool) -> None:
    """
    Entry point for extracting tables from ingested PDFs
    """

    logging.info('Running table extraction')
    start_time = time.time()

    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs

    for batch in load_table_metadata(db, buffer_size = n_jobs):
        t1 = time.time()
        logs = Parallel(n_jobs=n_jobs)(delayed(table_extraction)(table_metadata, skip) for table_metadata in batch)

        for log_list in logs:
            for log in log_list:
                logging.info(log)

        t2 = time.time()
        logging.info(f'Time for this batch: {t2-t1} s')

    end_time = time.time()

    logging.info(f'Completed table extractions')
    logging.info(f'Total time: {end_time - start_time} s')
    return


def load_table_metadata(db: pymongo.database.Database, buffer_size: int = 50, tables_per_job: int = 10) -> list:
    """
    Load documents buffer_size at a time
    """
    coll_raw_pdfs = db.raw_pdfs
    coll_detect_pages = db.detect_pages
    mongo_query_detect_pages = coll_detect_pages.find({},
                                                      {'page_height': 1, 'page_width': 1, 'pdf_name': 1, 'page_num': 1, 'detected_objs': 1},
                                                      no_cursor_timeout=True)
    table_data = []
    pdf_data = {}

    # Go through detect pages and get each page of a document
    for page in mongo_query_detect_pages:
        pdf_name = page['pdf_name']
        page_height = page['page_height']
        page_width = page['page_width']
        pdf_bytes = []

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

        # Get table coords and page of table in pdf
        for detected_obj in page['detected_objs']:
            if detected_obj[1][0][1] == "Table":
                page_num = str(page['page_num'])

                # Taking out coords, and translating them for camelot
                detected_coords = detected_obj[0]
                table_coords = []
                for coord in detected_coords:
                    table_coords.append(int(coord))

                x1 = int(table_coords[0] - 100)
                y1 = int(page_height - table_coords[1] + 100)
                x2 = int(table_coords[2] + 100)
                y2 = int(page_height - table_coords[3] - 100)
                coords_camelot = []
                coords_camelot.append(x1) if x1 > 0 else coords_camelot.append(0)
                coords_camelot.append(y1) if y1 < page_height else coords_camelot.append(page_height)
                coords_camelot.append(x2) if x2 < page_width else coords_camelot.append(page_width)
                coords_camelot.append(y2) if y2 > 0 else coords_camelot.append(0)p

                table_coords = str(table_coords)[1:-1]
                coords_camelot = str(coords_camelot)[1:-1]

                page_data = {"pdf_name": pdf_name, "bytes": pdf_bytes, "page_height": page_height,
                             "page_width": page_width, "coords": table_coords, "camelot_coords": coords_camelot,
                             "page_num": page_num}
                table_data.append(page_data)

                if len(table_data) == buffer_size*tables_per_job:
                    yield list(grouper(table_data, tables_per_job))
                    table_data = []

    yield list(grouper(table_data, tables_per_job))


def table_extraction(table_metadata: list, skip: bool) -> list:
    """
    Retrieve the tables from each table, and store them in mongodb.
    """

    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    coll_tables = db.tables_acc

    logs = []

    try:
        for table in table_metadata:
            pdf_name = table['pdf_name']
            table_page = table['page_num']
            table_height = table['page_height']
            table_width = table['page_width']
            table_coords = table['coords']
            table_coords2 = table['camelot_coords']

            logs.append(f'Processing {pdf_name}, page {table_page}, height {table_height}, width {table_width}, coords {table_coords} and {table_coords2}')
            # If document has already been scanned, ignore it based on skip settings
            if skip & do_skip(coll_tables, pdf_name):
                logs.append('Document previously extracted. Skipping')
            else:
                # Extract the tables
                table, extract_tables_logs = extract_tables(table)

                # Insert the data into mongodb
                insert_tables_mongo_logs = insert_tables_mongo(coll_tables, table)

                logs.extend(extract_tables_logs)
                logs.extend(insert_tables_mongo_logs)
    except Exception as e:
        logs.append(f'An error occurred: {e}')
    return logs


def do_skip(coll_tables: pymongo.collection.Collection, raw_pdf_name: str) -> bool:
    """
    Check if document is already scanned or not. If yes, skip it
    """
    return coll_tables.count_documents({'pdf_name': raw_pdf_name}, limit=1) != 0


def extract_tables(table: dict) -> list:
    """
    Extract each table using both Lattice and Stream. Compare and choose the best one.
    """
    logs = []
    pdf_name = table['pdf_name']
    pdf_bytes = table['bytes']
    table_coords = table['camelot_coords']
    table_page = table['page_num']

    logs.append('Extracting tables')

    try:
        file = open(pdf_name, 'wb')
        try:
            for line in BytesIO(pdf_bytes):
                file.write(line)
        except Exception:
            raise Exception('Could not create pdf from bytes')
        finally:
            file.close()

        tables_stream = camelot.read_pdf(pdf_name,
                                         pages=table_page,
                                         flavor='stream',
                                         table_regions=[table_coords],
                                         flag_size=True,
                                         strip_text=' .\n',
                                         edge_tol=25
                                         )

        tables_lattice = camelot.read_pdf(pdf_name,                                            
                                          pages=table_page,                                    
                                          flavor='lattice',                                    
                                          table_regions=[table_coords],                        
                                          split_text=True,                                     
                                          flag_size=True,                                      
                                          strip_text=' .\n',                                   
                                          line_scale=100,                                      
                                          shift_text=[''],                                     
                                          copy_text=['h', 'v']                                 
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

    finally:
        if path.exists(pdf_name):
            os.remove(pdf_name)

    table['table_df'] = pickle.dumps(table_df)
    table['flavor'] = flavor
    del table['camelot_coords']

    return [table, logs]


def insert_tables_mongo(coll_tables: pymongo.collection.Collection, detected_table: dict) -> list:
    """
    Insert tables into mongodb
    """
    result = coll_tables.insert_one(detected_table)
    return [f'Inserted table: {result}']


def grouper(iterable: iter, n: int, fillvalue=None):
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)


def is_picklable(obj):
    """
    Helpful function to debug whether objects are pickleable (requirement for multiprocessing)
    """
    try:
        pickle.dumps(obj)

    except pickle.PicklingError:
        return False
    return True
# print(f"Pickle table_extraction: {is_picklable(table_extraction)}")


@click.command()
@click.argument('num_processes')
@click.option('--skip/--no-skip', help="Don't try to update already extracted pdfs. Good to use if you ran into an "
                                       "error on ingestion and want to continue the ingestion")
def click_wrapper(num_processes: str, skip) -> None:
    print(skip)
    run_table_extraction(int(num_processes), skip)


if __name__ == '__main__':
    click_wrapper()

