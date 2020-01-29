# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from functools import lru_cache

import numpy as np
import torch

from fairseq.data import data_utils, Dictionary

from fairseq.data import BaseWrapperDataset, LRUCacheDataset


class CopyTokensDataset(BaseWrapperDataset):
    """
    A wrapper Dataset for masked language modeling.

    Input items are masked according to the specified masking probability.

    Args:
        dataset: Dataset to wrap.
        sizes: Sentence lengths
        vocab: Dictionary with the vocabulary and special tokens.
        pad_idx: Id of pad token in vocab
        mask_idx: Id of mask token in vocab
        return_masked_tokens: controls whether to return the non-masked tokens
            (the default) or to return a tensor with the original masked token
            IDs (and *pad_idx* elsewhere). The latter is useful as targets for
            masked LM training.
        seed: Seed for random number generator for reproducibility.
        mask_prob: probability of replacing a token with *mask_idx*.
        random_token_prob: probability of replacing a masked token with a
            random token from the vocabulary.
        mask_whole_words: only mask whole words. This should be a byte mask
            over vocab indices, indicating whether it is the beginning of a
            word. We will extend any mask to encompass the whole word.
        bpe: BPE to use for whole-word masking.
    """

    @classmethod
    def apply_mask(cls, dataset: torch.utils.data.Dataset, *args, **kwargs):
        """Return the source and target datasets for masked LM training."""
        dataset = LRUCacheDataset(dataset)
        return (
            LRUCacheDataset(cls(dataset, *args, **kwargs, return_masked_tokens=False)),
            LRUCacheDataset(cls(dataset, *args, **kwargs, return_masked_tokens=True)),
        )

    def __init__(
        self,
        dataset: torch.utils.data.Dataset,
        vocab: Dictionary,
        pad_idx: int,
        mask_idx: int,
        return_masked_tokens: bool = False,
        seed: int = 1,
        mask_prob: float = 0.33,
        mask_whole_words: torch.Tensor = None,
    ):
        assert 0.0 < mask_prob < 1.0

        self.dataset = dataset
        self.vocab = vocab
        self.pad_idx = pad_idx
        self.mask_idx = mask_idx
        self.return_masked_tokens = return_masked_tokens
        self.seed = seed
        self.mask_prob = mask_prob
        self.mask_whole_words = mask_whole_words

        self.epoch = 0

    def set_epoch(self, epoch, **unused):
        self.epoch = epoch

    @lru_cache(maxsize=8)
    def __getitem__(self, index: int):
        with data_utils.numpy_seed(self.seed, self.epoch, index):
            item = self.dataset[index]
            sz = len(item)

            assert self.mask_idx not in item, \
                'Dataset contains mask_idx (={}), this is not expected!'.format(
                    self.mask_idx,
                )

            word_begins_mask = self.mask_whole_words.gather(0, item)
            word_begins_idx = word_begins_mask.nonzero().view(-1)
            sz = len(word_begins_idx)
            words = np.split(word_begins_mask, word_begins_idx)[1:]
            assert len(words) == sz
            word_lens = list(map(len, words))

            start_ind = int(np.random.randint(0, sz-10 if sz-10 > 0 else 1, 1))
            end_ind = int(np.random.randint(start_ind+2 if start_ind+2 < sz else sz-1, start_ind+10 if start_ind+10 < sz else sz, 1))

            # select a span from the input
            original_sentence = item[word_begins_idx[start_ind].squeeze():word_begins_idx[end_ind].squeeze()] # +1??
            
            new_sentence = word_begins_idx[start_ind:end_ind]
            new_sentence_lens = word_lens[start_ind:end_ind]
            new_sentence_len = len(new_sentence)
            new_sentence_mask = np.full(new_sentence_len, False)
            num_mask = int(
                # add a random number for probabilistic rounding
                self.mask_prob * new_sentence_len + np.random.rand()
            )
            new_sentence_mask[np.random.choice(new_sentence_len, num_mask, replace=False)] = True


            if self.return_masked_tokens:
                # exit early if we're just returning the masked tokens
                # (i.e., the targets for masked LM training)
                new_sentence_mask = np.repeat(new_sentence_mask, new_sentence_lens)
                new_item = np.full(len(new_sentence_mask), self.pad_idx)
                new_item[new_sentence_mask] = original_sentence[torch.from_numpy(new_sentence_mask.astype(np.uint8)) == 1]
                pad_context = np.full(len(item), self.pad_idx)
                results = torch.cat((torch.from_numpy(pad_context), torch.from_numpy(new_item)))
                if len(results) > 511:
                    return torch.from_numpy(pad_context)
                return results

            new_sentence_mask = np.repeat(new_sentence_mask, new_sentence_lens)
            new_item = np.copy(original_sentence)
            new_item[new_sentence_mask] = self.mask_idx
            results = torch.cat((item, torch.from_numpy(new_item)))
            if len(results) > 511:
                return item
            final = torch.cat((item, torch.from_numpy(new_item)))

            return final

