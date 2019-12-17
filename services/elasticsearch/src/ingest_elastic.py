"""
Ingest to elasticsearch
"""
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from elasticsearch_dsl import Document, Text, connections
import pymongo
from pymongo import MongoClient
import os
import click

class Snippet(Document):
    content = Text()
    cls = Text()

    class Index:
        name = 'snippet'
        settings = {
                'number_of_shards': 1,
                'number_of_replicas': 0
        }


def ingest_elasticsearch(objects, code, sections, tableContexts, figureContexts, equationContexts):
    """
    Ingest some mongo collections to elasticsearch
    """
    connections.create_connection(hosts=['es01'])
    Snippet.init()
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    if objects:
        for batch in load_pages(db.objects, 100):
            for obj in batch:
                Snippet(_id=str(obj['_id']), cls=str(obj['class']), content=str(obj['content'])).save()

    if code:
        for batch in load_pages(db.code_objs, 100):
            for obj in batch:
                Snippet(_id=str(obj['_id']), cls=str(obj['class']), content=str(obj['content'])).save()

    if sections:
        for batch in load_pages(db.sections, 100):
            for obj in batch:
                Snippet(_id=str(obj['_id']), cls=str(obj['class']), content=str(obj['content'])).save()

    if tableContexts:
        for batch in load_pages(db.tableContexts, 100):
            for obj in batch:
                Snippet(_id=str(obj['_id']), cls=str(obj['class']), content=str(obj['content'])).save()

    if figureContexts:
        for batch in load_pages(db.figureContexts, 100):
            for obj in batch:
                Snippet(_id=str(obj['_id']), cls=str(obj['class']), content=str(obj['content'])).save()

    if equationContexts:
        for batch in load_pages(db.equationContexts, 100):
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

@click.command()
@click.option('--objects/--no-objects')
@click.option('--code/--no-code')
@click.option('--sections/--no-sections')
@click.option('--tables/--no-tables')
@click.option('--figures/--no-figures')
@click.option('--equations/--no-equations')
def ingest(objects, code, sections, tables, figures, equations):
    logging.info('Starting ingestion to elasticsearch')
    ingest_elasticsearch(objects, code, sections, tables, figures, equations)
    logging.info('Ending ingestion to elasticsearch')


if __name__ == '__main__':
    ingest()
