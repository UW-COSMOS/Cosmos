from retrieval.retriever import Retriever
from elasticsearch_dsl import Search, Q, query
from elasticsearch_dsl.connections import connections
from elasticsearch import RequestsHttpConnection
from elasticsearch_dsl import Document, Text, connections, Integer, Float, Keyword
from elasticsearch.helpers import bulk
import hashlib
import pandas as pd
import os
import logging
import base64

logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
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


class Page(Document):
    pdf_name = Text(fields={'raw': Keyword()})
    img_pth = Text(fields={'raw': Keyword()})
    page_num = Integer()
    dataset_id = Text(fields={'raw': Keyword()})

    postprocess_cls = Text(fields={'raw': Keyword()}, multi=True)
    postprocess_score = Float(multi=True)
    detect_cls = Text(fields={'raw': Keyword()}, multi=True)
    detect_score = Float(multi=True)
    pdf_dims = Float(multi=True)
    # arrays of arrays. not sure how to handle initializing properly, so I'll just let ES do it on ingest
#    bbox = Integer(multi=True)
#    classes = Text(multi=True)
#    scores = Float(multi=True)

    def get_id(self):
        '''
        Elasticsearch ingest process would be greatly improved by having a unique ID per object.

        TODO: is this actually unique and deterministic?
        '''
        return hashlib.sha1(f"{self.pdf_name}_{self.page_num}".encode('utf-8')).hexdigest()
    class Index:
        name = 'page'
        settings = {
            'number_of_shards': 1,
            'number_of_replicas': 0
        }

class ElasticPageRetriever(Retriever):
    def __init__(self, hosts=['localhost'], awsauth=None):
        self.hosts = hosts
        self.awsauth = awsauth

    def search(self, page_id='next_prediction'):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)

#        q = Q('match', dataset_id='documents')
        if page_id == "next_prediction":
            q = Q('function_score', functions=[query.SF("random_score")])
        else:
            q = Q('match', _id=page_id)

        s = Search(index='page').query(q)[0]
        logger.info("About to execute")
        resp = s.execute().to_dict()
        hit = resp['hits']['hits'][0]['_source']
        objects = zip(hit['bbox'], hit['postprocess_cls'], hit['postprocess_score'])
        pp_detected_objs = []
        for i in objects:
            pp_detected_objs.append({"bounding_box" : i[0], "class" : i[1], "confidence" : i[2], "annotated_class" : None, "obj_id" : -1})

        image_dir = '/cosmos_tmp/images'
        with open(os.path.join(image_dir, os.path.basename(hit['img_pth'])), 'rb') as imf:
            imbytes = base64.b64encode(imf.read()).decode('ascii')


        t = {
                "_id" : resp['hits']['hits'][0]['_id'],
                "page_height" : hit['pdf_dims'][3],
                "page_width" : hit['pdf_dims'][2],
                "pdf_id" : -1,
                "pdf_name" : hit['pdf_name'],
                "page_num" : hit["page_num"],
                "pp_detected_objs" : pp_detected_objs,
                "resize_bytes" : imbytes
                }
        return [t]

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

    def build_index(self, document_parquet):
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
        Page.init()
        # This is a parquet file to load from
        df = pd.read_parquet(document_parquet)
        unique_pages = df.groupby(['pdf_name', 'page_num', 'dataset_id', 'img_pth']).agg(lambda x: list(x))
        to_add = []
        for i, row in unique_pages.iterrows():
            to_add.append(Page(pdf_name=i[0],
                    page_num=i[1],
                    dataset_id=i[2],
                    img_pth=i[3],
                    pdf_dims=row['pdf_dims'][0].tolist(),
                    bbox=[j.tolist() for j in row['bounding_box']],
                    classes=[j.tolist() for j in row['classes']],
                    scores=[j.tolist() for j in row['scores']],
                    postprocess_cls=row['postprocess_cls'],
                    postprocess_score=row['postprocess_score'],
                    detect_cls=row['detect_cls'],
                    detect_score=row['detect_score']
                    ))
            if len(to_add) == 1000:
                bulk(connections.get_connection(), (upsert(d) for d in to_add))
                to_add = []
        bulk(connections.get_connection(), (upsert(d) for d in to_add))
        logger.info('Done building page index')

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
