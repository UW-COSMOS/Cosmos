# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import math

import torch
import torch.nn.functional as F

from fairseq import utils

from . import FairseqCriterion, register_criterion


@register_criterion('distill_masked_lm')
class DistilledMaskedLmLoss(FairseqCriterion):
    """
    Implementation for the loss used in distilled masked language model (MLM) training.
    """

    def __init__(self, args, task):
        super().__init__(args, task)
        self.attn_matching_loss = args.attn_matching_loss

    @staticmethod
    def add_args(parser):
        parser.add_argument('--attn-matching-loss', default=False, action='store_true',
                            help='use the attention masking loss')


    def forward(self, teacher_model, student_model, sample, reduce=True):
        """Compute the loss for the given sample.
        Returns a tuple with three elements:
        1) the loss
        2) the sample size, which is used as the denominator for the gradient
        3) logging outputs to display while training
        """
        if self.attn_matching_loss:
            attns = []
            student_attns = []

            def extract_attn(m, i, o):
                output, attn = o
                attns.append(attn)

            def student_attn_hook(m, i, o):
                output, attn = o
                student_attns.append(attn)

            '''
            IMPORTANT NOTE:
            This is getting the SOFTMAX of the attention weights. In order to switch to
            just the attention weights, you need to edit multihead_attention.py
            TODO: Make this easier to access for exp.
            '''
            for layer in teacher_model.decoder.sentence_encoder.layers:
                layer.register_forward_hook(extract_attn)
            for layer in student_model.decoder.sentence_encoder.layers:
                layer.register_forward_hook(student_attn_hook)
        # compute MLM loss
        with torch.no_grad():
            teacher_logits = teacher_model(**sample['net_input'], return_all_hiddens=False)[0]

        logits = student_model(**sample['net_input'], return_all_hiddens=False)[0]
        targets = student_model.get_targets(sample, [logits])
        mask_loss = F.nll_loss(
            F.log_softmax(
                logits.view(-1, logits.size(-1)),
                dim=-1,
                dtype=torch.float32,
            ),
            targets.view(-1),
            reduction='sum',
            ignore_index=self.padding_idx,
        )
        # log softmax the student prediction
        lsm = F.log_softmax(
                  logits.view(-1, logits.size(-1)),
                  dim=-1,
                  dtype=torch.float32,
              )
        lsm = lsm.unsqueeze(1)
        tl = F.log_softmax(
                teacher_logits.view(-1, teacher_logits.size(-1)),
                dim=-1,
                dtype=torch.float32)
        tl = tl.unsqueeze(2)
    
        teacher_loss = torch.sum(torch.bmm(lsm, tl))
        #teacher_loss = F.nll_loss(
        #        teacher_logits.view(-1, teacher_logits.size(-1)),
        #        reduction='sum',
        #        ignore_index=self.padding_idx,
        #)

        loss = mask_loss + teacher_loss

        if self.attn_matching_loss:
            for ind, student_attention in enumerate(student_attns):
                per_head_attn = student_attention[1]
                # TODO: Try this out
                #teacher_attentions = attns[ind*len(attns)/len(student_attns):(ind+1)*len(attns)/len(student_attns)]
                teacher_attention = attns[ind*3]
                per_head_teacher_attn = teacher_attention[1]
                attn_loss = F.mse_loss(per_head_attn, per_head_teacher_attn, reduction='sum')
                # normalize by num heads
                attn_loss = attn_loss / per_head_teacher_attn.shape[1]
                loss = loss + attn_loss


        sample_size = targets.ne(self.padding_idx).int().sum().item()

        logging_output = {
            'loss': utils.item(loss.data) if reduce else loss.data,
            'teacher_loss': utils.item(teacher_loss.data) if reduce else teacher_loss.data,
            'mask_loss': utils.item(mask_loss.data) if reduce else mask_loss.data,
            'ntokens': sample['ntokens'],
            'nsentences': sample['nsentences'],
            'sample_size': sample_size,
        }
        return loss, sample_size, logging_output

    @staticmethod
    def aggregate_logging_outputs(logging_outputs):
        """Aggregate logging outputs from data parallel training."""
        loss = sum(log.get('loss', 0) for log in logging_outputs)
        ntokens = sum(log.get('ntokens', 0) for log in logging_outputs)
        nsentences = sum(log.get('nsentences', 0) for log in logging_outputs)
        sample_size = sum(log.get('sample_size', 0) for log in logging_outputs)

        agg_output = {
            'loss': loss / sample_size / math.log(2),
            'ntokens': ntokens,
            'nsentences': nsentences,
            'sample_size': sample_size,
        }
        return agg_output
