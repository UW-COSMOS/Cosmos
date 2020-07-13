from torch import Dataset
import json
from transformers import BertTokenizerFast
from utils import collate


class TrainHierarchyExtractionDataset(Dataset):
    def __init__(self, data_path):
        with open(data_path) as rf:
            obj = json.load(rf)
        self.samples = obj['examples']
        self.label_map = obj['label_map']
        self.tokenizer = BertTokenizerFast.from_pretrained('bert-base-uncased')

    def __len__(self):
        return len(self.samples)

    def __getitem__(self, idx):
        sample = self.samples[idx]
        text = sample['text']
        x = self.tokenizer.encode_plus(text, return_tensors="pt", max_length=512)
        x = remove_padding(x)
        labels = sample['labels']
        return x, labels

    @classmethod
    def collate(cls, samples):
        return collate(samples)
        
