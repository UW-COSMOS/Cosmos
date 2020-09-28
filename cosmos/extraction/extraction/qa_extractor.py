from extraction.bert_qa_extractor.bert_qa_extractor import BertQAExtractor
from extraction.extractor import Extractor
from transformers import BertTokenizerFast, BertForQuestionAnswering
from transformers import AutoTokenizer, AutoModelForQuestionAnswering
from distributed.diagnostics.plugin import WorkerPlugin
from dask.distributed import Client, get_worker
import os
import click
import logging
logging.basicConfig(format='%(levelname)s :: %(filename) :: %(funcName)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
logger.setLevel(logging.WARNING)


class QAExtractor(Extractor):
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

    def extract(self, query, context):
        result = self.client.submit(QAExtractor._extract, query, context, resources={'qa': 1})
        result = result.result()
        return result

    @classmethod
    def _extract(cls, query, context):
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'qa_extractor' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise Exception('No QA plugin registered')
        model = dp.model
        answer, score = model.extract(query, context)
        return answer, score


class BertQAPlugin(WorkerPlugin):
    def __init__(self):
        model_path = os.environ.get("QA_MODEL_PATH")
        model = BertForQuestionAnswering.from_pretrained(model_path).to(os.environ.get('QA_DEVICE'))
        tokenizer = BertTokenizerFast.from_pretrained('bert-base-uncased')
        model.eval()
        self.model = BertQAExtractor(model, tokenizer)


@click.command()
def dask_setup(worker):
    worker._pending_plugins = [BertQAPlugin()]
