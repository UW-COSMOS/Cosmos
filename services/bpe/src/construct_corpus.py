"""
Create a corpus from all the text objects
"""
import pymongo
from pymongo import MongoClient
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import time
import click


def create_corpus():
    logging.info('Starting corpus construction')
    start_time = time.time()
    client = MongoClient(os.environ["DBCONNECT"])
    logging.info(f'Connected to client: {client}.')
    db = client.pdfs
    with open(os.path.join('corpus', 'corpus.txt'), 'w') as wf:
        for doc in db.partialSections.find(no_cursor_timeout=True):
            content = doc['content']
            wf.write(f'{content}\n')

        for doc in db.ocr_objs.find({'class': {'$in': ['Abstract', 'Figure Caption', 'Table Caption']}}, no_cursor_timeout=True):
            content = doc['content']
            wf.write(f'{content}\n')




@click.command()
def run():
    create_corpus()

if __name__ == '__main__':
    run()



