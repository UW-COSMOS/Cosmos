from torch.utils.data import Dataset
import torch
import json
from transformers import BertTokenizerFast
from bert_hierarchy_extractor.datasets.utils import collate, remove_padding


class TrainHierarchyExtractionDataset(Dataset):
    def __init__(self, data_path):
        with open(data_path) as rf:
            obj = json.load(rf)
        self.samples = obj['examples']
        self.label_map = obj['label_map']
        label_map_len = len(self.label_map)
        self.label_map['nullop'] = -1
        self.tokenizer = BertTokenizerFast.from_pretrained('bert-base-uncased')

    def __len__(self):
        return len(self.samples)

    def __getitem__(self, idx):
        sample = self.samples[idx]
        text = sample['text']
        x = self.tokenizer.encode_plus(text, return_tensors="pt", max_length=512, truncation=True, padding=True)
        x = remove_padding(x, 0)
        labels = sample['labels']
        labels = ['nullop'] + labels + ['nullop']
        labels = [self.label_map[l] for l in labels]
        labels = torch.LongTensor(labels)
        return x, labels

    @classmethod
    def collate(cls, samples):
        xs, labels = zip(*samples)
        xs = collate(xs)
        # We're going to specifically collate labels with -1 padding so we can mask it later
        labels = collate(labels, -1)
        return xs, labels
        
