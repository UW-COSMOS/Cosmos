from retrieval.retriever import Retriever
from elasticsearch_dsl import Search, Q
from elasticsearch_dsl.connections import connections
from elasticsearch import RequestsHttpConnection
from elasticsearch_dsl import Document, Text, connections, Integer, Float, Keyword
import pandas as pd
import logging

logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Object(Document):
    cls = Text(fields={'raw': Keyword()})
    detect_score = Float()
    postprocess_score = Float()
    dataset_id = Text(fields={'raw': Keyword()})
    header_content = Text()
    content = Text()
    area = Integer()
    pdf_name = Text(fields={'raw': Keyword()})
    img_pth = Text(fields={'raw': Keyword()})

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
    def __init__(self, hosts=['localhost'], awsauth=None):
        self.hosts = hosts
        self.awsauth = awsauth

    def search(self, query):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)
        q = Q('match', content=query)
        s = Search(index='fulldocument').query(q)[:30]
        response = s.execute()
        contexts = []
        for result in response:
            q = Q('match', pdf_name__raw=result['name'])
            s = Search(index='object').query(q)
            for context in s.scan():
                contexts.append({'id': context.meta.id, 'pdf_name': context['pdf_name'], 'content': context['content']})
        return contexts

    def build_index(self, document_parquet, section_parquet, tables_parquet, figures_parquet, equations_parquet):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)
        logger.info('Building elastic index')
        connections.create_connection(hosts=self.hosts)
        Object.init()
        FullDocument.init()
        # This is a parquet file to load from
        df = pd.read_parquet(document_parquet)
        for ind, row in df.iterrows():
            FullDocument(name=row['pdf_name'], dataset_id='none', content=row['content']).save()
        logger.info('Done building document index')
        df = pd.read_parquet(section_parquet)
        for ind, row in df.iterrows():
            Object(cls='Section',
                   dataset_id=row['dataset_id'],
                   content=row['content'],
                   header_content=row['section_header'],
                   area=50,
                   detect_score=row['detect_score'],
                   postprocess_score=row['postprocess_score'],
                   pdf_name=row['pdf_name'],
                   ).save()
        logger.info('Done building section index')

        if tables_parquet != '':
            df = pd.read_parquet(tables_parquet)
            for ind, row in df.iterrows():
                Object(cls='Table',
                       dataset_id=row['dataset_id'],
                       content=row['content'],
                       header_content=row['caption_content'],
                       area=50,
                       detect_score=row['detect_score'],
                       postprocess_score=row['postprocess_score'],
                       pdf_name=row['pdf_name'],
                       img_pth=row['img_pth'],
                       ).save()
            logger.info('Done building tables index')
        if figures_parquet != '':
            df = pd.read_parquet(figures_parquet)
            for ind, row in df.iterrows():
                Object(cls='Figure',
                       dataset_id=row['dataset_id'],
                       content=row['content'],
                       header_content=row['caption_content'],
                       area=50,
                       detect_score=row['detect_score'],
                       postprocess_score=row['postprocess_score'],
                       pdf_name=row['pdf_name'],
                       img_pth=row['img_pth'],
                       ).save()
            logger.info('Done building figures index')

        if equations_parquet != '':
            df = pd.read_parquet(equations_parquet)
            for ind, row in df.iterrows():
                Object(cls='Equation',
                       dataset_id=row['dataset_id'],
                       content=row['content'],
                       header_content='',
                       area=50,
                       detect_score=row['detect_score'],
                       postprocess_score=row['postprocess_score'],
                       pdf_name=row['pdf_name'],
                       img_pth=row['img_pth'],
                       ).save()
            logger.info('Done building equations index')

        logger.info('Done building object index')

    def delete(self, dataset_id):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)
        s = Search(index='fulldocument')
        q = Q()
        q = q & Q('match', dataset_id__raw=dataset_id)
        result = s.query(q).delete()
        logger.info(result)
        s = Search(index='object')
        q = Q()
        q = q & Q('match', dataset_id__raw=dataset_id)
        result = s.query(q).delete()
        logger.info(result)

    def rerank(self, query, contexts):
        raise NotImplementedError('ElasticRetriever does not rerank results')
