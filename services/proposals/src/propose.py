"""
Add proposals
"""

from pymongo import MongoClient
import os
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)


def propose():
    """
    On event trigger, run the proposals script
    """
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    # Open a cursor that will watch for inserts on raw_pdfs
    cursor = db.raw_pdfs.watch()
    while True:
        doc = next(cursor)
        logging.info(doc)

if __name__ == '__main__':
    propose()


