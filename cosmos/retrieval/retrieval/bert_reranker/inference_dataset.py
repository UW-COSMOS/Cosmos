from torch.utils.data import Dataset
from typing import NamedTuple
from retrieval.utils import Encoding, remove_padding, collate
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class InferenceQuery(NamedTuple):
    query: str
    context: str

class InferenceDataset(Dataset):
    def __init__(self, query, contexts, tokenizer, max_length=512):
        self.load_samples(query, contexts)
        self.max_length = max_length
        self.tokenizer = tokenizer

    def load_samples(self, query, contexts):
        self.samples = []
        for c in contexts:
            self.samples.append(InferenceQuery(query, c))

    def __len__(self):
        return len(self.samples)

    def __getitem__(self, idx):
        sample = self.samples[idx]
        query, context = sample
        x = self.tokenizer.encode_plus(
            query, context, return_tensors="pt", max_length=self.max_length, truncation=True, padding=True
        )
        x = remove_padding(x, self.tokenizer.pad_token_id)
        return x

    @classmethod
    def collate(cls, samples):
        padding_value = 0 # should probably replace this with a partial application in the collate call, but whatever for now
        return collate(samples, padding_value)

