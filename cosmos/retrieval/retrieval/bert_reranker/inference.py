from retrieval.bert_reranker.inference_dataset import InferenceDataset
from transformers import BertTokenizerFast
from retrieval.utils import cudafy
from torch.utils.data import DataLoader
import glob
import os
import numpy as np
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

class Inference:
    def __init__(self, model, model_pth, bsz, device, num_workers):
        self.model = model.to(device)
        self.model.eval()
        self.bsz = bsz
        self.num_workers = num_workers
        self.tokenizer = BertTokenizerFast.from_pretrained(model_pth)
        logger.error(self.tokenizer.pad_token_id)
                                          

    def infer(self, query, contexts):
        dataset = InferenceDataset(query, contexts, self.tokenizer)
        dataloader = DataLoader(dataset, 
                                batch_size=self.bsz, 
                                shuffle=False, 
                                collate_fn=InferenceDataset.collate,)
        full_scores = []
        full_ids = []
        full_docnames = []
        for batch in dataloader:
            xs, ids, docnames = batch
            xs = cudafy(xs)
            logits = self.model(**xs)[0]
            scores = logits[:, 1]
            scores = scores.squeeze()
            scores = list(scores.detach().cpu().numpy().flatten())
            full_scores.extend(scores)
            full_ids.extend(ids)
            full_docnames.extend(docnames)
        cs = zip(contexts, full_scores, full_ids, full_docnames)
        cs = sorted(cs, key=lambda x: x[1], reverse=True)
        cs = [{'query': query, 'context': q['content'], 'score': s, 'id': i, 'docname': d} for q, s, i, d in cs]
        return cs

