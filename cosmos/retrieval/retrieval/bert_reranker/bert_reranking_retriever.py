from retrieval.retriever import Retriever
from retrieval.bert_reranker.inference import Inference
from transformers import BertForSequenceClassification
from distributed.diagnostics.plugin import WorkerPlugin
import os
import click
from dask.distributed import Client
from dask.distributed import get_worker
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

class BertRerankingRetriever(Retriever):
    def __init__(self, client):
        if type(client) == str:
            logger.info('Input is a str, inferring to be scheduler address. Initializing client')
            self.addr = client
            self.client = Client(client)#, serializers=['msgpack', 'dask'], deserializers=['msgpack', 'dask'])
            logger.info(self.client)
        else:
            self.client = client
            self.addr = None
        
    def __del__(self):
        if self.addr is not None:
            self.client.close()

    def search(self, query):
        raise NotImplementedError('Reranking does not search')

    def rerank(self, query, contexts):
        result = self.client.submit(BertRerankingRetriever._rerank, query, contexts, resources={'retrieval': 1})
        result = result.result()
        return result

    @classmethod
    def _rerank(cls, query, contexts):
        contexts = [c for c in contexts if c != '']
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'bert_reranking_retriever' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise Exception('No reranking plugin registered')
        model = dp.model
        result = model.infer(query, contexts)
        #contexts, scores = zip(*result)
        #result = {'contexts': contexts, 'scores': scores}
        return result

    def build_index(self, path):
        raise NotImplementedError('Reranking does not require building an index')

    def delete(self, path):
        raise NotImplementedError('Reranking does not require building an index')


class BertRerankingPlugin(WorkerPlugin):
    def __init__(self):
        model_path = os.environ.get("RERANKING_MODEL_PATH")
        device = os.environ.get("RERANKING_DEVICE")
        bsz = int(os.environ.get("RERANKING_BATCH_SIZE"))
        num_workers = int(os.environ.get("RERANKING_NUM_WORKERS"))
        model = BertForSequenceClassification.from_pretrained(model_path)
        self.model = Inference(model, model_path, bsz, device, num_workers)

@click.command()
def dask_setup(worker):
    worker._pending_plugins = [BertRerankingPlugin()]

