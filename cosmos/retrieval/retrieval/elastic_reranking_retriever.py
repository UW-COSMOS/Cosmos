from retrieval.retriever import Retriever
from retrieval.bert_reranker.bert_reranking_retriever import BertRerankingRetriever
from retrieval.elastic_retriever import ElasticRetriever
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
import os


class ElasticRerankingRetriever(Retriever):
    def __init__(self, client, hosts=[os.environ["ELASTIC_ADDRESS"]]):
        self.elastic_retriever = ElasticRetriever(hosts)
        self.reranker = BertRerankingRetriever(client)


    def search(self,
               query,
               ndocs=10,
               page=0,
               cls=None,
               detect_min=None,
               postprocess_min=None,
               return_all=False,
               get_count=False):
        logger.error('Starting search.')
        contexts = self.elastic_retriever.search(query,
                                                 ndocs=ndocs,
                                                 page=page,
                                                 cls=cls,
                                                 detect_min=detect_min,
                                                 postprocess_min=postprocess_min)
        if get_count:
            pdf_count = set()
            for c in contexts:
                pdf_count.add(c['pdf_name'])
            return len(pdf_count)
        logger.info('Starting reranking')
        results = self.rerank(query, contexts)
        logger.info('Finished reranking')
        if return_all:
            return results
        doc_set = set()
        final_results = []
        for result in results:
            if result['docname'] in doc_set:
                continue
            doc_set.add(result['docname'])
            final_results.append(result)
        final_results = [r['id'] for r in final_results]
        final_results = [self.elastic_retriever.get_object(i) for i in final_results]
        final_results = [
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
                'context_keywords': '',
                'context_summary': '',
                'context_content': '',
                'context_id': obj.meta.id
            } for obj in final_results
        ]
        return final_results


    def rerank(self, query, contexts):
        return self.reranker.rerank(query, contexts)

    def build_index(self, document_parquet, entities_parquet, section_parquet, tables_parquet, figures_parquet, equations_parquet):
        self.elastic_retriever.build_index(document_parquet, entities_parquet, section_parquet, tables_parquet, figures_parquet, equations_parquet)

    def delete(self, dataset_id):
        self.elastic_retriever.delete(dataset_id)

