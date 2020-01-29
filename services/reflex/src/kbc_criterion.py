import numpy as np
import torch
import torch.nn.functional as F
import math
from fairseq import utils

from fairseq.criterions import FairseqCriterion, register_criterion


@register_criterion('kbc_criterion')
class KBCCriterion(FairseqCriterion):

    def __init__(self, args, task):
        super().__init__(args, task)

    def forward(self, model, sample, reduce=True):
        # First compute the sentence loss

        logits, _ = model(**sample['net_input'], features_only=True, classification_head_name='sentence_classification_head',)
        targets = model.get_targets(sample, [logits]).view(-1)
        sample_size = targets.numel()
        l1 = F.nll_loss(F.log_softmax(logits, dim=-1, dtype=torch.float32), targets, reduction='sum')
        preds = logits.max(dim=1)[1]
        ncorrect = (preds == targets).sum().item()



        # Second, compute the masked loss

        logits = model(**sample['net_input'], return_all_hiddens=False)[0]
        targets = sample['mask_target'].view(-1)
        l2 = F.nll_loss(F.log_softmax(logits.view(-1, logits.size(-1)), dim=-1, dtype=torch.float32,), targets, reduction='sum', ignore_index=self.padding_idx)

        sum_loss = l1 + l2
        ntokens = sample['ntokens']
        nsentences = sample_size

        logging_output = {
            'loss': utils.item(sum_loss.data) if reduce else sum_loss.data,
            'ntokens': ntokens,
            'nsentences': sample_size,
            'sample_size': sample_size,
            'ncorrect': ncorrect,
            'concept_loss': utils.item(l1.data) if reduce else l1.data,
            'masked_loss': utils.item(l2.data) if reduce else l2.data,
        }
        return sum_loss, sample_size, logging_output

    @staticmethod
    def aggregate_logging_outputs(logging_outputs):
        loss_sum = sum(log.get('loss', 0) for log in logging_outputs)
        ntokens = sum(log.get('ntokens', 0) for log in logging_outputs)
        nsentences = sum(log.get('nsentences', 0) for log in logging_outputs)
        sample_size = sum(log.get('sample_size', 0) for log in logging_outputs)
        ncorrect = sum(log.get('ncorrect', 0) for log in logging_outputs)
        concept_loss = sum(log.get('concept_loss', 0) for log in logging_outputs)
        masked_loss = sum(log.get('masked_loss', 0) for log in logging_outputs)

        agg_output = {
            'loss':  loss_sum / sample_size / math.log(2),
            'ntokens': ntokens,
            'nsentences': nsentences,
            'sample_size': sample_size,
            'accuracy': ncorrect/nsentences,
            'concept_loss': concept_loss / sample_size / math.log(2),
            'masked_loss': masked_loss / sample_size / math.log(2)
        }
        return agg_output




