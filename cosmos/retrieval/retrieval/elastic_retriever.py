from retrieval.retriever import Retriever
from elasticsearch_dsl import Search, Q
from elasticsearch_dsl.connections import connections
from elasticsearch import RequestsHttpConnection
from elasticsearch_dsl import Document, Text, connections, Integer, Float, Keyword
from elasticsearch.helpers import bulk
import hashlib
import pandas as pd
import logging

logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

def upsert(doc):
    d = doc.to_dict(True)
    d['_op_type'] = 'update'
    d['doc'] = d['_source']
    d['_index'] = doc.Index().name
    d['doc_as_upsert'] = True
    d['_id'] = str(doc.get_id())
    del d['_source']
    return d

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

    def get_id(self):
        '''
        Elasticsearch ingest process would be greatly improved by having a unique ID per object.

        TODO: is this actually unique and deterministic?
        '''
        return hashlib.sha1(f"{self.cls}{self.detect_score}{self.postprocess_score}{self.dataset_id}{self.header_content}{self.content}{self.pdf_name}".encode('utf-8')).hexdigest()

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
    def get_id(self):
        return self.name.replace(".pdf", "")

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

    def search(self, query, ndocs=30, page=0, cls=None, detect_min=None, postprocess_min=None, get_count=False, final=False, inclusive=False, document_filter_terms=[], docids=[], obj_id=None):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)
        logging.info(f"document_filter_terms: {document_filter_terms}")
        # TODO: document_filter_terms
        # Run a  query against 'fulldocument' index.
        doc_filter = False
        dq = Q()
        logging.info(f"docids: {docids}")

        if len(docids) > 0:
            dq = dq & Q('bool', should=[Q('match_phrase', name=f"{i}.pdf") for i in docids])
            doc_filter=True
        if len(document_filter_terms) > 0:
            dq = dq & Q('bool', must=[Q('match_phrase', content=i) for i in document_filter_terms])
            doc_filter=True

        if doc_filter:
            ds = Search(index='fulldocument')
            ds = ds.query(dq)
            pdf_names = []
            for resp in ds.scan():
                pdf_names.append(resp['name'])
            logging.info(f"{len(pdf_names)} pdfs found")

        q = Q()
        if query is None:
            query_list = []
        elif "," in query:
            query_list = query.split(",")
        else:
            query_list = [query]
        if inclusive:
            q = q & Q('bool', must=[Q('match_phrase', content=i) for i in query_list])
        else:
            q = q & Q('bool', should=[Q('match_phrase', content=i) for i in query_list])
        start = page * ndocs
        end = start + ndocs
        s = Search(index='object')
        if cls is not None:
            s = s.filter('term', cls__raw=cls)
        if detect_min is not None:
            s = s.filter('range', detect_score={'gte': detect_min})
        if postprocess_min is not None:
            s = s.filter('range', postprocess_score={'gte': postprocess_min})

        if doc_filter > 0:
            s = s.filter('terms', pdf_name__raw=pdf_names)

        if obj_id is not None:
            q = q & Q('ids', values=[obj_id])

        if get_count:
            return s.query(q).count()

        s = s.query(q)[start:end]

        response = s.execute()
        final_results = [r.meta.id for r in response]
        final_results = [self.get_object(i) for i in final_results]
        if final:
            contexts = [
                {
                    'header': {},
                    'pdf_name': obj.pdf_name,
                    'children': [{
                        'id': obj.meta.id,
                        'bytes': obj.img_pth,
                        'cls': obj.cls,
                        'postprocessing_confidence': obj.postprocess_score,
                        'base_confidence': obj.detect_score,
                        'content': obj.content,
                        'header_content': obj.header_content,
                    }],
                } for obj in final_results
            ]
        logger.error(f'Found {len(contexts)} contexts')
        return contexts

    def get_object(self, id):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)
        return Object.get(id=id)

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
        if document_parquet != '':
            to_add = []
            df = pd.read_parquet(document_parquet)
            for ind, row in df.iterrows():
                to_add.append(FullDocument(name=row['pdf_name'], dataset_id=row['dataset_id'], content=row['content']))
                if len(to_add) == 1000:
                    bulk(connections.get_connection(), (upsert(d) for d in to_add))
                    to_add = []
            bulk(connections.get_connection(), (upsert(d) for d in to_add))
            logger.info('Done building document index')

        if section_parquet != '':
            df = pd.read_parquet(section_parquet)
            to_add = []
            for ind, row in df.iterrows():
                to_add.append(
                        Object(cls='Section',
                            dataset_id=row['dataset_id'],
                            content=row['content'],
                            header_content=row['section_header'],
                            area=50,
                            detect_score=row['detect_score'],
                            postprocess_score=row['postprocess_score'],
                            pdf_name=row['pdf_name'],
                       ))
                if len(to_add) == 1000:
                    bulk(connections.get_connection(), (upsert(d) for d in to_add))
                    to_add = []
            bulk(connections.get_connection(), (upsert(d) for d in to_add))

            logger.info('Done building section index')

        if tables_parquet != '':
            df = pd.read_parquet(tables_parquet)
            to_add = []
            for ind, row in df.iterrows():
                to_add.append(Object(cls='Table',
                    dataset_id=row['dataset_id'],
                    content=row['content'],
                    header_content=row['caption_content'],
                    area=50,
                    detect_score=row['detect_score'],
                    postprocess_score=row['postprocess_score'],
                    pdf_name=row['pdf_name'],
                    img_pth=row['img_pth'],
                    ))
                if len(to_add) == 1000:
                    bulk(connections.get_connection(), (upsert(d) for d in to_add))
                    to_add = []
            bulk(connections.get_connection(), (upsert(d) for d in to_add))
            logger.info('Done building tables index')
        if figures_parquet != '':
            df = pd.read_parquet(figures_parquet)
            to_add = []
            for ind, row in df.iterrows():
                to_add.append(Object(cls='Figure',
                       dataset_id=row['dataset_id'],
                       content=row['content'],
                       header_content=row['caption_content'],
                       area=50,
                       detect_score=row['detect_score'],
                       postprocess_score=row['postprocess_score'],
                       pdf_name=row['pdf_name'],
                       img_pth=row['img_pth'],
                       ))
                if len(to_add) == 1000:
                    bulk(connections.get_connection(), (upsert(d) for d in to_add))
                    to_add = []
            bulk(connections.get_connection(), (upsert(d) for d in to_add))
            logger.info('Done building figures index')

        if equations_parquet != '':
            df = pd.read_parquet(equations_parquet)
            to_add = []
            for ind, row in df.iterrows():
                to_add.append(Object(cls='Equation',
                       dataset_id=row['dataset_id'],
                       content=row['content'],
                       header_content='',
                       area=50,
                       detect_score=row['detect_score'],
                       postprocess_score=row['postprocess_score'],
                       pdf_name=row['pdf_name'],
                       img_pth=row['img_pth'],
                       ))
                if len(to_add) == 1000:
                    bulk(connections.get_connection(), (upsert(d) for d in to_add))
                    to_add = []
            bulk(connections.get_connection(), (upsert(d) for d in to_add))
            logger.info('Done building equations index')

        logger.info('Done building object index')

    def count(self, index):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)
        s = Search(index=index)
        return s.count()

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
