from retrieval.retriever import Retriever
from elasticsearch_dsl import Search, Q
from elasticsearch_dsl.connections import connections
from elasticsearch_dsl import Document, Text, connections, Integer, Float, Keyword
import pickle
import pandas as pd
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

class Object(Document):
    cls = Text(fields={'raw': Keyword()})
    score = Float()
    dataset_id = Text(fields={'raw': Keyword()})
    content = Text()
    area = Integer()
    pdf_name = Text(fields={'raw': Keyword()})
    page_num = Integer()

    class Index:
        name = 'object'
        settings = {
                'number_of_shards': 1,
                'number_of_replicas': 0
        }

class FullDocument(Document):
    dataset_id = Text(fields={'raw': Keyword()})
    content = Text()
    name = Text(fields={'raw': Keyword()})

    class Index:
        name = 'fulldocument'
        settings = {
                'number_of_shards': 1,
                'number_of_replicas': 0
        }

class ElasticRetriever(Retriever):
    def __init__(self, hosts=['localhost']):
        self.hosts = hosts

    def search(self, query):
        connections.create_connection(hosts=self.hosts)
        q = Q('match', content=query)
        s = Search(index='fulldocument').query(q)[:10]
        response = s.execute()
        contexts = []
        for result in response:
            name = result['name']
            q = Q('match', pdf_name__raw=result['name'])
            s = Search(index='object').query(q)
            for context in s.scan():
                contexts.append(context['content'])
        return contexts

    def build_index(self, input_path):
        logger.info('Building elastic index')
        connections.create_connection(hosts=self.hosts)
        Object.init()
        FullDocument.init()
        if input_path[-3:] == 'uet':
            # This is a parquet file to load from
            df = pd.read_parquet(input_path)
            groups = df.groupby(['pdf_name', 'dataset_id'])
            for name, group in groups:
                pdf_text = ''
                for ind, row in group.iterrows():
                    pdf_text += '\n'
                    pdf_text += row['content']
                FullDocument(name=name[0], dataset_id=name[1], content=pdf_text).save()
            logger.info('Done building document index')
            for ind, row in df.iterrows():
                tlx, tly, brx, bry = row['bounding_box']
                area = (brx - tlx) * (bry - tly)
                Object(cls=row['classes'][0],
                        score=row['scores'][0],
                        dataset_id=name[1],
                        content=row['content'],
                        area=area,
                        page_num=row['page_num'],
                        pdf_name=row['pdf_name'],
                        ).save()
            logger.info('Done building object index')
        else:
            raise NotImplementedError('Only parquet files are currently supported')

    def delete(self, dataset_id):
        connections.create_connection(hosts=self.hosts)
        s = Search(index='fulldocument')
        q = Q()
        q = q & Q('match', dataset_id=dataset_id)
        result = s.query(q).delete()
        logger.info(result)
        s = Search(index='object')
        q = Q()
        q = q & Q('match', dataset_id=dataset_id)
        result = s.query(q).delete()
        logger.info(result)

    def rerank(self, query, contexts):
        raise NotImplementedError('ElasticRetriever does not rerank results')


