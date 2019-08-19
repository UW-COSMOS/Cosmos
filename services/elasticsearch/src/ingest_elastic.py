"""
Ingest to elasticsearch
"""
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from elasticsearch_dsl import Document, Text, connections
import pymongo
from pymongo import MongoClient
import os

class Snippet(Document):
    content = Text()
    cls = Text()

    class Index:
        name = 'snippet'
        settings = {
                'number_of_shards': 1,
                'number_of_replicas': 0
        }


def ingest_elasticsearch():
    """
    Ingest some mongo collections to elasticsearch
    """
    connections.create_connection(hosts=['es01'])
    Snippet.init()
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    for batch in load_pages(db.ocr_objs, 100):
        for obj in batch:
            Snippet(_id=str(obj['_id']), cls=str(obj['class']), content=str(obj['content'])).save()
    for batch in load_pages(db.code_objs, 100):
        for obj in batch:
            Snippet(_id=str(obj['_id']), cls=str(obj['class']), content=str(obj['content'])).save()
    Snippet._index.refresh()


def load_pages(coll, buffer_size):
    """
    Load pages from a given collection
    """
    current_docs = []
    for doc in coll.find().batch_size(buffer_size):
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs

if __name__ == '__main__':
    logging.info('Starting ingestion to elasticsearch')
    ingest_elasticsearch()
    logging.info('Ending ingestion to elasticsearch')
