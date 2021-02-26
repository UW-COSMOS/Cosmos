from retrieval.retriever import Retriever
from elasticsearch_dsl import Search, Q, query, UpdateByQuery
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

def get_object_id(obj):
    '''
    Elasticsearch ingest process would be greatly improved by having a unique ID per object.

    '''
    return hashlib.sha1(f"{obj['cls']}{obj['detect_score']}{obj['postprocess_score']}{obj['dataset_id']}{obj['header_content']}{obj['content']}{obj['pdf_name']}".encode('utf-8')).hexdigest()

def upsert(doc):
    d = doc.to_dict(True)
    d['_op_type'] = 'update'
    d['doc'] = d['_source']
    d['_index'] = doc.Index().name
    d['doc_as_upsert'] = True
    d['_id'] = str(doc.get_id())
    del d['_source']
    return d


class DetectedObject(Document):
    pdf_name = Text(fields={'raw': Keyword()})
    img_pth = Text(fields={'raw': Keyword()})
    page_num = Integer()
    dataset_id = Text(fields={'raw': Keyword()})

    bbox = Integer(multi=True)
    classes = Text(multi=True)
    scores = Float(multi=True)
    postprocess_cls = Text()
    postprocess_score = Float()
    detect_cls = Text()
    detect_score = Float()

    def get_id(self):
        '''
        Elasticsearch ingest process would be greatly improved by having a unique ID per object.

        TODO: is this actually unique and deterministic?
        '''
        return hashlib.sha1(f"{self.pdf_name}_{self.page_num}_{self.bbox}".encode('utf-8')).hexdigest()

    class Index:
        name = 'detected-objects'
        settings = {
            'number_of_shards': 1,
            'number_of_replicas': 0
        }

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
        resp = s.execute().to_dict()
        hit = resp['hits']['hits'][0]['_source']

        pdf_name = hit['pdf_name']
        page_num = hit['page_num']

        pp_detected_objs = []
        q =  Q('bool', must=[Q('match_phrase', pdf_name=pdf_name), Q('match_phrase', page_num=page_num)])
        s = Search(index='detected-objects').query(q)
        for obj in s.scan():
            pp_detected_objs.append({"bounding_box" : list(obj["bbox"]), "class" : obj["postprocess_cls"], "confidence": -1, "detect_cls": obj["detect_cls"], "detect_score" : obj["detect_score"], "postprocess_confidence" : obj["postprocess_score"], "annotated_class" : None, "obj_id" : str(obj.meta.id)})

        image_dir = '/cosmos_tmp/images'
        with open(os.path.join(image_dir, os.path.basename(hit['img_pth']).replace("_pad", "")), 'rb') as imf:
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
        DetectedObject.init()
        # This is a parquet file to load from
        df = pd.read_parquet(document_parquet)
        unique_pages = df.groupby(['pdf_name', 'page_num', 'dataset_id', 'img_pth']).agg(lambda x: list(x))
        to_add = []
        objects_to_add = []
        for i, row in unique_pages.iterrows():

            # TODO: want to write bbox, classes, scores, postprocess_cls, postprocess_score, detect_cls, detect_score to detected-objects index

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

            for j, _ in enumerate(row['bounding_box']):
                objects_to_add.append(DetectedObject(pdf_name=i[0],
                    page_num=i[1],
                    dataset_id=i[2],
                    bbox=row['bounding_box'][j].tolist(),
                    classes=row['classes'][j].tolist(),
                    scores=row['scores'][j].tolist(),
                    postprocess_cls=row['postprocess_cls'][j],
                    postprocess_score=row['postprocess_score'][j],
                    detect_cls=row['detect_cls'][j],
                    detect_score=row['detect_score'][j]
                    ))

            if len(to_add) == 1000:
                bulk(connections.get_connection(), (upsert(d) for d in to_add))
                bulk(connections.get_connection(), (upsert(o) for o in objects_to_add))
                to_add = []
                objects_to_add = []
        bulk(connections.get_connection(), (upsert(d) for d in to_add))
        logger.info('Done building page index')

    def detected_object_annotate(self, object_id, field, value):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts)
        to_update = DetectedObject.get(id=object_id)

        to_update.update(**{field: value})
        return

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
        s = Search(index='object')
        q = Q()
        q = q & Q('match', dataset_id__raw=dataset_id)
        result = s.query(q).delete()

    def rerank(self, query, contexts):
        raise NotImplementedError('ElasticRetriever does not rerank results')
