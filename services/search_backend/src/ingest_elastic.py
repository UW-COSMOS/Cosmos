import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from elasticsearch_dsl import Document, Text, connections

class Snippet(Document):
    content = Text()

    class Index:
        name = 'snippet'
        settings = {
                'number_of_shards': 1,
                'number_of_replicas': 0
        }


def ingest_elasticsearch():
    connections.create_connection()
    Snippet.init()
    client = MongoClient(os.environ["DBCONNECT"])
    db = client.pdfs
    for batch in load_pages(db, 100):
        for obj in batch:
            Snippet(_id=str(obj['_id']), content=str(obj['content'])).save()
    Snippet._index.refresh()


def load_pages(db, buffer_size):
    """
    """
    current_docs = []
    for doc in db.ocr_objs.find().batch_size(buffer_size):
        current_docs.append(doc)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs

if __name__ == '__main__':
    logging.info('Starting ingestion to elasticsearch')
    ingest_elasticsearch()
    logging.info('Ending ingestion to elasticsearch')
