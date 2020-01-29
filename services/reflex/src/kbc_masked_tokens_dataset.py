
from functools import lru_cache

import numpy as np
import torch

from fairseq.data import data_utils, Dictionary

from fairseq.data import BaseWrapperDataset, LRUCacheDataset

class KBCMaskTokensDataset(BaseWrapperDataset):

    @classmethod
    def apply_mask(cls, dataset, *args, **kwargs):
        return (LRUCacheDataset(cls(dataset, *args, **kwargs, return_masked_tokens=False)),
                LRUCacheDataset(cls(dataset, *args, **kwargs, return_masked_tokens=True)),
               )

    def __init__(self, objects_dataset, subjects_dataset, concepts_dataset, contexts_dataset, vocab, pad_idx, mask_idx, subj_idx, obj_idx, seed, return_masked_tokens=False):
        self.dataset = objects_dataset
        self.subjects_dataset = subjects_dataset
        self.concepts_dataset = concepts_dataset
        self.contexts_dataset = contexts_dataset
        
        self.vocab = vocab
        self.pad_idx = pad_idx
        self.mask_idx = mask_idx
        self.subj_idx = subj_idx
        self.obj_idx = obj_idx
        self.return_masked_tokens = return_masked_tokens
        self.seed = seed

        self.epoch = 0

    def set_epoch(self, epoch, **unused):
        self.epoch = epoch

    @lru_cache(maxsize=8)
    def __getitem__(self, index):
        with data_utils.numpy_seed(self.seed, self.epoch, index):
            object = self.dataset[index]
            subject = self.subjects_dataset[index]
            concept = self.concepts_dataset[index]
            context = self.contexts_dataset[index]

            if self.return_masked_tokens:
                subj_pad = np.full(len(subject), self.pad_idx)
                concept_pad = np.full(len(concept), self.pad_idx)
                context_pad = np.full(len(context), self.pad_idx)
                subj_t = torch.from_numpy(subj_pad)
                concept_t = torch.from_numpy(concept_pad)
                context_t = torch.from_numpy(context_pad)
                return torch.cat([subj_t, concept_t, object, torch.tensor([self.pad_idx]), context_t])
                #return torch.cat([subj_t, concept_t, object, context_t])

            obj_mask = torch.from_numpy(np.full(len(object), self.mask_idx))
            return torch.cat([subject, concept, obj_mask, torch.tensor([self.vocab.eos()]), context])
            #return torch.cat([subject, concept, obj_mask, context])


            


