"""
"""

import sqlite3
import logging
from drqa import retriever
import os
import click

logger = logging.getLogger()
logger.setLevel(logging.INFO)
fmt = logging.Formatter('%(asctime)s: [ %(message)s ]', '%m/%d/%Y %I:%M:%S %p')
console = logging.StreamHandler()
console.setFormatter(fmt)
logger.addHandler(console)


@click.command()
@click.option('--db-dump')
@click.option('--output-file')
def write_wiki(db_dump, output_file):
    conn = sqlite3.connect(db_dump)
    with open(output_file, 'w') as of:
        c = conn.cursor()
        c.execute("SELECT * FROM documents")
        while True:
            row = c.fetchone()
            if row is None:
                break
            doc = row[1]
            of.write(f'{doc}\n')


if __name__ == '__main__':
    write_wiki()

    

