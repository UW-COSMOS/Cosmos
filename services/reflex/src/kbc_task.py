

import json
import os

import numpy as np
import torch
import torch.nn.functional as F

from fairseq.data import (
    data_utils,
    Dictionary,
    encoders,
    IdDataset,
    ListDataset,
    NestedDictionaryDataset,
    NumSamplesDataset,
    NumelDataset,
    RawLabelDataset,
    RightPadDataset,
    SortDataset,
)
from fairseq.tasks import FairseqTask, register_task

from .kbc_masked_tokens_dataset import KBCMaskTokensDataset

MAX_LEN = 412
WINDOW_SHIFT = 128
VAL_OUTPUT = 'val_output_test1.csv'


@register_task('kbc')
class KBCTask(FairseqTask):

    @staticmethod
    def add_args(parser):
        parser.add_argument('data', metavar='DIR', help='path to data')
        parser.add_argument('--init-token', type=int, default=None, help='add token at the beginning of each batch item')
        parser.add_argument('--regression-target', action='store_true', default=False)

    def __init__(self, args, vocab):
        super().__init__(args)
        self.vocab = vocab
        self.mask = vocab.add_symbol('<mask>')
        self.subj = 'SUBJECT'#vocab.add_symbol('<subj>')
        self.obj = 'OBJECT'#vocab.add_symbol('<obj>')
        self.bpe = encoders.build_bpe(args)
        self.write_file = open(VAL_OUTPUT, 'w')

    @classmethod
    def load_dictionary(cls, filename):
        dictionary = Dictionary.load(filename)
        dictionary.add_symbol('<mask>')
        #dictionary.add_symbol('<subj>')
        #dictionary.add_symbol('<obj>')
        return dictionary

    @classmethod
    def setup_task(cls, args, **kwargs):
        vocab = cls.load_dictionary(os.path.join(args.data, 'dict.txt'))
        print('| dictionary: {} types'.format(len(vocab)))
        return cls(args, vocab)

    def load_dataset(self, split, combine=False, epoch=0, **kwargs):
        def binarize(s, ob=None, sub=None, append_bos=False, eos=True):
            if self.bpe is None:
                raise Exception('BPE is none, something is wrong')
            s = self.bpe.encode(s)
            tokens = self.vocab.encode_line(s, append_eos=eos, add_if_not_exist=False).long()
            sequences = []
            if len(tokens) >= MAX_LEN:
                curr_loc = 0
                next_loc = MAX_LEN
                while next_loc <= len(tokens):
                    seq = tokens[curr_loc:next_loc]
                    curr_loc += WINDOW_SHIFT
                    next_loc += WINDOW_SHIFT
                    seq_len = len(seq)
                    seq = torch.cat([seq, torch.tensor([self.vocab.eos()])])
                    m = seq.ge(len(self.vocab))
                    if m.any():
                        print(seq)
                    sequences.append(seq)
                last_seq = tokens[curr_loc:]
                if len(last_seq) > 0:
                    sequences.append(last_seq)
            else:
                sequences.append(tokens)

                
            if append_bos and self.args.init_token is not None:
                for idx, seq in enumerate(sequences):
                    sequences[idx] = torch.cat([seq.new([self.args.init_token]), seq])
            return sequences

        data_path = os.path.join(self.args.data, f'{split}.jsonl')
        if not os.path.exists(data_path):
            raise FileNotFoundError('Cannot find data: {}'.format(data_path))

        def is_beginning_of_word(i):
            if i < self.source_dictionary.nspecial:
                # special elements are always considered beginnings
                return True
            tok = self.source_dictionary[i]
            if tok.startswith('madeupword'):
                return True
            try:
                return self.bpe.is_beginning_of_word(tok)
            except ValueError:
                return True

        mask_whole_words = torch.ByteTensor(list(
            map(is_beginning_of_word, range(len(self.source_dictionary)))
        ))


        src_objects  = []
        src_objects_len = []
        src_subjects = []
        src_subjects_len = []
        src_concepts = []
        src_concepts_len = []
        src_contexts = []
        src_contexts_len = []
        labels = []

        with open(data_path) as h:
            for line in h:
                example = json.loads(line.strip())
                concept = example['concept']
                subject = example['subject']
                object = example['object']
                context = example['context']
                label = example['label']


                object_enc = binarize(object, eos=False)[0]
                subject_enc = binarize(subject, append_bos=True, eos=False)[0]
                c = binarize(concept, eos=False)[0]
                con = binarize(context, eos=True)
                for context in con:
                    src_objects.append(object_enc)
                    src_objects_len.append(len(object_enc))
                    src_subjects.append(subject_enc)
                    src_subjects_len.append(len(subject_enc))
                    src_concepts.append(c)
                    src_concepts_len.append(len(c))
                    src_contexts.append(context)
                    src_contexts_len.append(len(context))
                    labels.append(int(label))

        src_objects_len = np.array(src_objects_len)
        src_subjects_len = np.array(src_subjects_len)
        src_concepts_len = np.array(src_concepts_len)
        src_contexts_len = np.array(src_contexts_len)

        src_objects = ListDataset(src_objects, src_objects_len)
        src_subjects = ListDataset(src_subjects, src_subjects_len)
        src_concepts = ListDataset(src_concepts, src_concepts_len)
        src_contexts = ListDataset(src_contexts, src_contexts_len)


        src_dataset, target_dataset = KBCMaskTokensDataset.apply_mask(src_objects,
                src_subjects,
                src_concepts,
                src_contexts,
                self.source_dictionary,
                pad_idx=self.source_dictionary.pad(),
                mask_idx=self.mask,
                subj_idx=self.subj,
                obj_idx=self.obj,
                seed=self.args.seed)



        dataset = {
                'id': IdDataset(),
                'nsentences': NumSamplesDataset(),
                'ntokens': NumelDataset(src_dataset, reduce=True),
                'net_input': {
                    'src_tokens': RightPadDataset(src_dataset, pad_idx=self.source_dictionary.pad()),
                    'src_lengths': NumelDataset(src_dataset, reduce=False),
                },
                'target': RawLabelDataset(labels),
                'mask_target': RightPadDataset(target_dataset, pad_idx=self.source_dictionary.pad()),
            }

        dataset = NestedDictionaryDataset(
                    dataset,
                    sizes=[src_dataset.sizes]
                )

        with data_utils.numpy_seed(self.args.seed):
            dataset = SortDataset(
                    dataset,
                    sort_order=src_dataset.sizes)

        print('| Loaded {} with {} samples'.format(data_path, len(dataset)))
        self.datasets[split] = dataset
        return self.datasets[split]

    def build_model(self, args):
        from fairseq import models
        model = models.build_model(args, self)

        model.register_classification_head(
                'sentence_classification_head',
                num_classes=2,
        )

        return model

    def valid_step(self, sample, model, criterion):
        model.eval()
        with torch.no_grad():
            loss, sample_size, logging_output = criterion(model, sample)

        logits, _ = model(**sample['net_input'], features_only=True, classification_head_name='sentence_classification_head',)
        preds = logits.max(dim=1)[1][0].item()
        targets = model.get_targets(sample, [logits]).view(-1)[0].item()

        masked_index = (sample['net_input']['src_tokens'][0, :] == self.mask).nonzero().squeeze()
        
        features = model(**sample['net_input'], features_only=False, return_all_hiddens=False)[0]
        logits2 = features[0, masked_index, :].squeeze()
        prob = logits2.softmax(dim=0)
        if len(prob.shape) == 1:
            prob = prob.unsqueeze(0)
        values, indices = prob.topk(k=1, dim=1)

        str_list = []
        for ind in indices:
            s = self.source_dictionary.string(ind)
            try:
                decode = self.bpe.decode(s)
                str_list.append(decode)
            except Exception as e:
                str_list.append(s)

        mask_target_list = []
        for ind in sample['mask_target'][0].unsqueeze(1):
            if ind.item() == 1:
                continue
            s = self.source_dictionary.string(ind)
            try:
                decode = self.bpe.decode(s)
                mask_target_list.append(decode)
            except Exception as e:
                mask_target_list.append(s)

        sample_list = []
        for ind in sample['net_input']['src_tokens'][0].unsqueeze(1):
            s = self.source_dictionary.string(ind)
            try:
                decode = self.bpe.decode(s)
                sample_list.append(decode)
            except Exception as e:
                sample_list.append(s)
        

        self.write_file.write(f'Prediction: {preds} || Target: {targets} || Mask predition: {" ".join(str_list)} || Mask Target: {" ".join(mask_target_list)} || {" ".join(sample_list)}\n')

        return loss, sample_size, logging_output

    @property
    def source_dictionary(self):
        return self.vocab

    @property
    def target_dictionary(self):
        return self.vocab









