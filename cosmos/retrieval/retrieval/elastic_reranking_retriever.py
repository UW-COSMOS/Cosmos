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

    def search(self, query):
        contexts = self.elastic_retriever.search(query)
        return self.rerank(query, contexts)

    def rerank(self, query, contexts):
        return self.reranker.rerank(query, contexts)

    def build_index(self, document_parquet, section_parquet):
        self.elastic_retriever.build_index(document_parquet, section_parquet)

    def delete(self, dataset_id):
        self.elastic_retriever.delete(dataset_id)

