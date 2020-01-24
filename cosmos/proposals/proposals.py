"""
Entry script for ingesting PDFs, creating the first object with its connected components
"""

# Logging config
import logging
logging.basicConfig(format='%(levelname) :: %(asctime)s :: %(message)s', level=logging.DEBUG)

import click
import multiprocessing as mp
import tempfile
import time

@click.command()
@click.argument('pdf_path')
def run_proposals(pdf_dir: str):
    """
    Entry point for ingesting PDF documents
    """
    logging.info('Running proposal creation')
    start_time = time.time()
    # Make a tmp directory to let ghostscript write pngs
    with tempfile.TemporaryDirectory() as img_tmp:



    end_time = time.time()
    logging.info(f'End running proposal creation. Total time: {end_time - start_time} s')


if __name__ == '__main__':
    run_proposals()

