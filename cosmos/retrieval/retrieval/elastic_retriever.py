from retrieval.retriever import Retriever
from elasticsearch_dsl import Search, Q
from elasticsearch_dsl.connections import connections
from elasticsearch import RequestsHttpConnection
from elasticsearch_dsl import Document, Text, connections, Integer, Float, Keyword, Join
from elasticsearch.helpers import bulk
import hashlib
import pandas as pd
import hashlib
import logging

logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

def get_size(bbs):
    area = 0
    try:
        if not isinstance(bbs[0], float):
            for subbb in bbs:
                area+=get_size(subbb)
        else:
            area += (bbs[3] - bbs[1]) * (bbs[2] - bbs[0])
        return area
    except IndexError:
        return 0
    except:
        print(f"Issue getting area. {sys.exc_info()}")
        return -1

def combine_contents(fields):
    combined_contents = ' '.join([i for i in fields if i is not None])
    return combined_contents

def upsert(doc):
    d = doc.to_dict(True)
    d['_op_type'] = 'update'
    d['doc'] = d['_source']
    d['_index'] = doc.Index().name
    d['doc_as_upsert'] = True
    d['_id'] = str(doc.get_id())
    del d['_source']
    return d

class EntityObjectIndex(Document):
    """
    We need to put entities and the objects associated on the same shard, so we are going have them inherit
    from this index class
    """
    entity_object = Join(relations={"entity": "object"})

    @classmethod
    def _matches(cls, hit):
        return False

    class Index:
        name = 'eo-site'
        settings = {
            'number_of_shards': 1,
            'number_of_replicas': 0
        }

class Entity(EntityObjectIndex):
    canonical_id = Text(fields={'raw': Keyword()})
    name = Text(fields={'raw': Keyword()})
    description = Text()
    types = Keyword(multi=True)
    aliases = Text(multi=True)
    dataset_id = Text(fields={'raw': Keyword()})

    @classmethod
    def _matches(cls, hit):
        """ Use Entity class for parent documents """
        return hit["_source"]["entity_object"] == "entity"

    @classmethod
    def search(cls, **kwargs):
        return cls._index.search(**kwargs).filter("term", entity_object="entity")

    def get_id(self):
        '''
        Elasticsearch ingest process would be greatly improved by having a unique ID per object.
        TODO: is this actually unique and deterministic?
        '''
        return hashlib.sha1(f"{self.canonical_id}{self.name}{self.description}{self.types}{self.aliases}{self.dataset_id}".encode('utf-8')).hexdigest()

    def add_object(self, cls, dataset_id, content, header_content, context_from_text, area, detect_score, postprocess_score, pdf_name, img_path=None, commit=True):
        obj = Object(
            # required make sure the answer is stored in the same shard
            _routing=self.meta.id,
            # since we don't have explicit index, ensure same index as self
            _index=self.meta.index,
            # set up the parent/child mapping
            entity_object={"name": "object", "parent": self.meta.id},
            # pass in the field values
            cls=cls,
            dataset_id=dataset_id,
            content=content,
            header_content=header_content,
            full_content=combine_content([content, header_content, context_from_text]),
            local_content=combine_content([content, header_content]),
            area=area,
            detect_score=detect_score,
            postprocess_score=postprocess_score,
            pdf_name=pdf_name,
            img_path=img_path
        )
        if commit:
            obj.save()
        return obj

    def search_objects(self):
        # search only our index
        s = Object.search()
        # filter for answers belonging to us
        s = s.filter("parent_id", type="object", id=self.meta.id)
        # add routing to only go to specific shard
        s = s.params(routing=self.meta.id)
        return s

    def get_objects(self):
        """
        Get objects either from inner_hits already present or by searching
        elasticsearch.
        """
        if "inner_hits" in self.meta and "object" in self.meta.inner_hits:
            return self.meta.inner_hits.object.hits
        return list(self.search_objects())

    def save(self, **kwargs):
        self.entity_object = "entity"
        return super(Entity, self).save(**kwargs)


class Object(EntityObjectIndex):
    cls = Text(fields={'raw': Keyword()})
    detect_score = Float()
    postprocess_score = Float()
    dataset_id = Text(fields={'raw': Keyword()})
    header_content = Text()
    content = Text()
    context_from_text = Text()
    full_content = Text()
    local_content = Text()
    area = Integer()
    pdf_name = Text(fields={'raw': Keyword()})
    img_pth = Text(fields={'raw': Keyword()})

    def get_id(self):
        '''
        Elasticsearch ingest process would be greatly improved by having a unique ID per object.

        '''
        return hashlib.sha1(f"{self.cls}{self.detect_score}{self.postprocess_score}{self.dataset_id}{self.header_content}{self.content}{self.pdf_name}".encode('utf-8')).hexdigest()

    @classmethod
    def _matches(cls, hit):
        """ Use Object class for child documents with child name 'object' """
        return (
                isinstance(hit["_source"]["entity_object"], dict)
                and hit["_source"]["entity_object"].get("name") == "object"
        )

    @classmethod
    def search(cls, **kwargs):
        return cls._index.search(**kwargs).exclude("term", entity_object="entity")

    def save(self, **kwargs):
        # set routing to parents id automatically
        self.meta.routing = self.entity_object.parent
        return super(Object, self).save(**kwargs)


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

    # TODO: oof, I don't really want to pass in more crap here.
    def search(self, query, entity_search=False, ndocs=30, page=0, cls=None, detect_min=None, postprocess_min=None, get_count=False, final=False, inclusive=False, document_filter_terms=[], docids=[], obj_id=None, dataset_id=None, content_field="local_content"):
        if self.awsauth is not None:
            connections.create_connection(hosts=self.hosts,
                                          http_auth=self.awsauth,
                                          use_ssl=True,
                                          verify_certs=True,
                                          connection_class=RequestsHttpConnection
                                          )
        else:
            connections.create_connection(hosts=self.hosts, timeout=20)

        if entity_search:
            es = Entity.search()
            q = Q('match', name=query)
            response = es.query(q).execute()
            logger.info('Done finding entity')
            for hit in response:
                s = hit.search_objects()
                if cls is not None:
                    s = s.filter('term', cls__raw=cls)
                if detect_min is not None:
                    s = s.filter('range', detect_score={'gte': detect_min})
                if postprocess_min is not None:
                    s = s.filter('range', postprocess_score={'gte': postprocess_min})
                start = page * ndocs
                end = start + ndocs
                contexts = []
                cq = Q('match', content=query)
                os_response = s.query(cq)[start:end].execute()
                final_results = [r.meta.id for r in os_response]
                final_results = [self.get_object(i) for i in final_results]
        else:
            # TODO: pull this out and do it above the entity level
            # Run a  query against 'fulldocument' index.
            doc_filter = False
            dq = Q()

            if len(docids) > 0:
                dq = dq & Q('bool', should=[Q('match_phrase', name=f"{i}.pdf") for i in docids])
                doc_filter=True
            if len(document_filter_terms) > 0:
                dq = dq & Q('bool', must=[Q('match_phrase', **{content_field:i}) for i in document_filter_terms])
                doc_filter=True

            if doc_filter:
                ds = Search(index='fulldocument')
                ds = ds.query(dq)
                pdf_names = []
                for resp in ds.scan():
                    pdf_names.append(resp['name'])

            q = Q()
            if query is None:
                query_list = []
            elif "," in query:
                query_list = query.split(",")
            else:
                query_list = [query]
            if inclusive:
                q = q & Q('bool', must=[Q('match_phrase', **{content_field:i}) for i in query_list])
            else:
                q = q & Q('bool', should=[Q('match_phrase', **{content_field:i}) for i in query_list])

            start = page * ndocs
            end = start + ndocs
            s = Search(index='eo-site')
            if cls is not None:
                s = s.filter('term', cls__raw=cls)

            if dataset_id is not None:
                s = s.filter('term', dataset_id__raw=dataset_id)

            # Filter figures/tables, enforcing size not approaching full-page. TODO: Overkill and will cut legitimate full-page tables
            q = q & Q('bool', must_not=[Q('bool', must=[Q('match_phrase', cls__raw='Figure'), Q('range', area={'gte': 2000000})])])
            q = q & Q('bool', must_not=[Q('bool', must=[Q('match_phrase', cls__raw='Table'), Q('range', area={'gte': 2000000})])])

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
                        'context_from_text' : obj.context_from_text if 'context_from_text' in obj else None
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
        try:
            obj = Object.get(id=id)
        except:
            obj = None
        return obj

    def build_index(self, document_parquet, entities_parquet, section_parquet, tables_parquet, figures_parquet, equations_parquet):
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
        index_template = EntityObjectIndex._index.as_template("base")
        index_template.save()
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

        if entities_parquet != '':
            df = pd.read_parquet(entities_parquet)
            for ind, row in df.iterrows():
                Entity(canonical_id=row['id'],
                       name=row['name'],
                       description=row['description'],
                       types=row['types'].tolist(),
                       aliases=row['aliases'].tolist(),
                       dataset_id=row['dataset_id']).save()
            logger.info('Done building entities index')

        if section_parquet != '':
            if entities_parquet != '': # TODO: better way to recognize that we're using entities
                df = pd.read_parquet(section_parquet)
                for ind, row in df.iterrows():
                    entities = row['ents_linked']
                    to_add = []
                    for entity in entities:
                        es = Entity.search()
                        es = es.filter('term', canonical_id__raw=entity)
                        response = es.execute()
                        for hit in response:
                            to_add.append(hit.add_object('Section',
                                           row['dataset_id'],
                                           row['content'],
                                           row['section_header'],
                                           get_size(row['obj_bbs']),
                                           row['detect_score'],
                                           row['postprocess_score'],
                                           row['pdf_name'],
                                           commit=False))
                            if len(to_add) == 100:
                                bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                                to_add = []
                    if to_add == []: continue
                    bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                    to_add = []
            else:
                df = pd.read_parquet(section_parquet)
                to_add = []
                for ind, row in df.iterrows():
                    to_add.append(
                            Object(cls='Section',
                                dataset_id=row['dataset_id'],
                                content=row['content'],
                                header_content=row['section_header'],
                                context_from_text=row['context_from_text'] if 'context_from_text' in row else None,
                                full_content=combine_contents([row['content'], row['section_header']]),
                                local_content=combine_contents([row['content'], row['section_header']]),
                                area=get_size(row['obj_bbs']),
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
            if entities_parquet != '': # TODO: better way to recognize that we're using entities
                for ind, row in df.iterrows():
                    entities = row['ents_linked']
                    for entity in entities:
                        es = Entity.search()
                        es = es.filter('term', canonical_id__raw=entity)
                        response = es.execute()
                        for hit in response:
                            to_add.append(hit.add_object('Table',
                                           row['dataset_id'],
                                           row['content'],
                                           row['caption_content'],
                                           get_size(row['obj_bbs']),
                                           row['detect_score'],
                                           row['postprocess_score'],
                                           row['pdf_name'],
                                           row['img_pth'],
                                           commit=False))
                            if len(to_add) == 100:
                                bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                                to_add = []
                    if to_add == []: continue
                    bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                    to_add = []
            else:
                for ind, row in df.iterrows():
                    to_add.append(Object(cls='Table',
                        dataset_id=row['dataset_id'],
                        content=row['content'],
                        header_content=row['caption_content'],
                        context_from_text=row['context_from_text'] if 'context_from_text' in row else None,
                        full_content=combine_contents([row['content'], row['caption_content'], row['context_from_text'] if 'context_from_text' in row else None]),
                        local_content=combine_contents([row['content'], row['caption_content']]),
                        area=get_size(row['obj_bbs']),
                        detect_score=row['detect_score'],
                        postprocess_score=row['postprocess_score'],
                        pdf_name=row['pdf_name'],
                        img_pth=row['img_pth'],
                        ))
                    if len(to_add) == 1000:
                        bulk(connections.get_connection(), (upsert(d) for d in to_add))
                        to_add = []
            bulk(connections.get_connection(), (upsert(d) for d in to_add))
            to_add = []
            logger.info('Done building tables index')

        if figures_parquet != '':
            df = pd.read_parquet(figures_parquet)
            to_add = []
            if entities_parquet != '': # TODO: better way to recognize that we're using entities
                for ind, row in df.iterrows():
                    entities = row['ents_linked']
                    for entity in entities:
                        es = Entity.search()
                        es = es.filter('term', canonical_id__raw=entity)
                        response = es.execute()
                        for hit in response:
                            to_add.append(hit.add_object('Figure',
                                           row['dataset_id'],
                                           row['content'],
                                           row['caption_content'],
                                           get_size(row['obj_bbs']),
                                           row['detect_score'],
                                           row['postprocess_score'],
                                           row['pdf_name'],
                                           row['img_pth'],
                                           commit=False))
                            if len(to_add) == 100:
                                bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                                to_add = []
                    if to_add == []: continue
                bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                to_add = []
            else:
                for ind, row in df.iterrows():
                    to_add.append(Object(cls='Figure',
                           dataset_id=row['dataset_id'],
                           content=row['content'],
                           header_content=row['caption_content'],
                           context_from_text=row['context_from_text'] if 'context_from_text' in row else None,
                           full_content=combine_contents([row['content'], row['caption_content'], row['context_from_text'] if 'context_from_text' in row else None]),
                           local_content=combine_contents([row['content'], row['caption_content']]),
                           area=get_size(row['obj_bbs']),
                           detect_score=row['detect_score'],
                           postprocess_score=row['postprocess_score'],
                           pdf_name=row['pdf_name'],
                           img_pth=row['img_pth'],
                           ))
                    if len(to_add) == 1000:
                        bulk(connections.get_connection(), (upsert(d) for d in to_add))
                        to_add = []
                bulk(connections.get_connection(), (upsert(d) for d in to_add))
                to_add = []
            logger.info('Done building figures index')

        if equations_parquet != '':
            df = pd.read_parquet(equations_parquet)
            to_add = []
            if entities_parquet != '': # TODO: better way to recognize that we're using entities
                for ind, row in df.iterrows():
                    entities = row['ents_linked']
                    for entity in entities:
                        es = Entity.search()
                        es = es.filter('term', canonical_id__raw=entity)
                        response = es.execute()
                        for hit in response:
                            to_add.append(hit.add_object('Equation',
                                           row['dataset_id'],
                                           row['content'],
                                           None,
                                           get_size(row['equation_bb']),
                                           row['detect_score'],
                                           row['postprocess_score'],
                                           row['pdf_name'],
                                           row['img_pth'],
                                           commit=False))
                            if len(to_add) == 100:
                                bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                                to_add = []
                    if to_add == []: continue
                    bulk(connections.get_connection(), (o.to_dict(True) for o in to_add), request_timeout=20, max_retries=1)
                to_add = []
            else:
                for ind, row in df.iterrows():
                    to_add.append(Object(cls='Equation',
                           dataset_id=row['dataset_id'],
                           content=row['content'],
                           header_content='',
                           context_from_text=row['context_from_text'] if 'context_from_text' in row else None,
                           full_content=combine_contents([row['content']]),
                           local_content=combine_contents([row['content']]),
                           area=get_size(row['equation_bb']),
                           detect_score=row['detect_score'],
                           postprocess_score=row['postprocess_score'],
                           pdf_name=row['pdf_name'],
                           img_pth=row['img_pth'],
                           ))
                    if len(to_add) == 500:
                        bulk(connections.get_connection(), (upsert(d) for d in to_add))
                        to_add = []
                bulk(connections.get_connection(), (upsert(d) for d in to_add))
            logger.info('Done building equations index')
        logger.info('Done building object index')

    def count(self, index, dataset_id=None):
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
        if dataset_id is not None:
            s = s.filter('term', dataset_id__raw=dataset_id)

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
        s = Search(index='eo-site')
        q = Q()
        q = q & Q('match', dataset_id__raw=dataset_id)
        result = s.query(q).delete()
        logger.info(result)

    def rerank(self, query, contexts):
        raise NotImplementedError('ElasticRetriever does not rerank results')
