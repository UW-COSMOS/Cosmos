# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import torch
import numpy as np
import re
import string
import collections


def _print_top_k(value_max_probs, index_max_probs, vocab, mask_topk, index_list, max_printouts = 10):
    result = []
    msg = "\n| Top{} predictions\n".format(max_printouts)
    for i in range(mask_topk):
        filtered_idx = index_max_probs[i].item()

        if index_list is not None:
            # the softmax layer has been filtered using the vocab_subset
            # the original idx should be retrieved
            idx = index_list[filtered_idx]
        else:
            idx = filtered_idx

        log_prob = value_max_probs[i].item()
        word_form = vocab[idx]
        if word_form.startswith('_'):
            word_form = word_form[1:-1]

        if i < max_printouts:
            msg += "{:<8d}{:<20s}{:<12.3f}\n".format(
                i,
                word_form,
                log_prob
            )
        element = {'i' : i, 'token_idx': idx, 'log_prob': log_prob, 'token_word_form': word_form}
        result.append(element)
    return result, msg

def normalize_answer(s):
    """Lower text and remove punctuation, articles and extra whitespace."""
    def remove_articles(text):
        regex = re.compile(r'\b(a|an|the)\b', re.UNICODE)
        return re.sub(regex, ' ', text)
    def white_space_fix(text):
        return ' '.join(text.split())
    def remove_punc(text):
        exclude = set(string.punctuation)
        return ''.join(ch for ch in text if ch not in exclude)
    def lower(text):
        return text.lower()
    return white_space_fix(remove_articles(remove_punc(lower(s))))

def get_tokens(s):
  if not s: return []
  return normalize_answer(s).split()

def compute_exact(a_gold, a_pred):
    return int(normalize_answer(a_gold) == normalize_answer(a_pred))

def compute_f1(a_gold, a_pred):
    gold_toks = get_tokens(a_gold)
    pred_toks = get_tokens(a_pred)
    common = collections.Counter(gold_toks) & collections.Counter(pred_toks)
    num_same = sum(common.values())
    if len(gold_toks) == 0 or len(pred_toks) == 0:
        # If either is no-answer, then F1 is 1 if they agree, 0 otherwise
        return int(gold_toks == pred_toks)
    if num_same == 0:
        return 0
    precision = 1.0 * num_same / len(pred_toks)
    recall = 1.0 * num_same / len(gold_toks)
    f1 = (2 * precision * recall) / (precision + recall)
    return f1


def calculate_em_f1(target, prediction, anchor=None):
    print(target)
    print(prediction)
    f1 = compute_f1(target, prediction)
    em = compute_exact(target, prediction)

    if anchor is not None:
        prediction_tokens = get_tokens(prediction)
        target_tokens = get_tokens(target)
        anchor_tokens = get_tokens(anchor)
        pred_too_large = 0
        pred_too_small = 0
        should_be_empty = 0
        should_be_not_empty = 0
        anchor_outside = 0
        mismatch = 0
        is_error = 1 if em == 0 else 0
        if is_error:
            # This is an error, categorize it
            if len(prediction_tokens) == 0 and len(target_tokens) > 0:
                should_be_not_empty = 1
            else:
                if len(prediction_tokens) > 0 and len(target_tokens) == 0:
                    should_be_empty = 1
                else:
                    if len(anchor_tokens) == 0:
                        # This was filtered to have no answer, we'll consider it outside
                        anchor_outside = 1

                    elif anchor_tokens[0] not in target_tokens:
                        anchor_outside = 1
                    else:
                        if len(prediction_tokens) > len(target_tokens):
                            pred_too_large = 1
                        elif len(prediction_tokens) < len(target_tokens):
                            pred_too_small = 1
                        else:
                            mismatch = 1

        return em, f1, is_error, pred_too_large, pred_too_small, should_be_empty, should_be_not_empty, anchor_outside, mismatch
    else:
        prediction_tokens = get_tokens(prediction)
        target_tokens = get_tokens(target)
        no_overlap = 0
        larger_by_1 = 0
        larger_by_2 = 0
        larger_by_3 = 0
        larger_by_4 = 0
        larger_by_5_or_more = 0
        is_error = 1 if em == 0 else 0
        if is_error:
            if target_tokens[0] not in prediction_tokens:
                no_overlap = 1
            else:
                len_mismatch = len(prediction_tokens) - len(target_tokens)
                if len_mismatch == 1:
                    larger_by_1 = 1
                elif len_mismatch == 2:
                    larger_by_2 = 1
                elif len_mismatch == 3:
                    larger_by_3 = 1
                elif len_mismatch == 4:
                    larger_by_4 = 1
                elif len_mismatch >= 5:
                    larger_by_5_or_more = 1
                else:
                    raise Exception('Fix me')

        return em, f1, is_error, no_overlap, larger_by_1, larger_by_2, larger_by_3, larger_by_4, larger_by_5_or_more

def calculate_em_f1_zsre(target, prediction, anchor=None):
    f1 = compute_f1(target, prediction)
    em = compute_exact(target, prediction)

    if anchor is not None:
        prediction_tokens = get_tokens(prediction)
        target_tokens = get_tokens(target)
        anchor_tokens = get_tokens(anchor)
        pred_too_large = 0
        pred_too_small = 0
        should_be_empty = 0
        should_be_not_empty = 0
        anchor_outside = 0
        mismatch = 0
        is_error =  0

        return em, f1, is_error, pred_too_large, pred_too_small, should_be_empty, should_be_not_empty, anchor_outside, mismatch
    else:
        prediction_tokens = get_tokens(prediction)
        target_tokens = get_tokens(target)
        no_overlap = 0
        larger_by_1 = 0
        larger_by_2 = 0
        larger_by_3 = 0
        larger_by_4 = 0
        larger_by_5_or_more = 0
        is_error = 0
    
        return em, f1, is_error, no_overlap, larger_by_1, larger_by_2, larger_by_3, larger_by_4, larger_by_5_or_more





def get_ranking(log_probs, masked_indices, vocab, is_masked_probs=False, label_index = None, index_list = None, topk = 1000, P_AT = 10, print_generation=True, return_masked_topk=False):

    experiment_result = {}

    # score only first mask
    if not is_masked_probs:
        masked_indices = masked_indices[:1]

        masked_index = masked_indices[0]
        log_probs = log_probs[masked_index]

    value_max_probs, index_max_probs = torch.topk(input=log_probs,k=topk,dim=0)
    index_max_probs = index_max_probs.numpy().astype(int)
    value_max_probs = value_max_probs.detach().numpy()

    result_masked_topk, return_msg = _print_top_k(value_max_probs, index_max_probs, vocab, topk, index_list)
    if return_masked_topk:
        return result_masked_topk
    experiment_result['topk'] = result_masked_topk

    if print_generation:
        print(return_msg)

    MRR = 0.
    P_AT_X = 0.
    P_AT_1 = 0.
    PERPLEXITY = None

    if label_index is not None:

        # check if the labe_index should be converted to the vocab subset
        if index_list is not None:
            label_index = index_list.index(label_index)

        query = torch.full(value_max_probs.shape, label_index, dtype=torch.long).numpy().astype(int)
        ranking_position = (index_max_probs==query).nonzero()

        # LABEL PERPLEXITY
        tokens = torch.from_numpy(np.asarray(label_index))
        label_perplexity = log_probs.gather(
            dim=0,
            index=tokens,
        )
        PERPLEXITY = label_perplexity.item()

        if len(ranking_position) >0 and ranking_position[0].shape[0] != 0:
            rank = ranking_position[0][0] + 1

            # print("rank: {}".format(rank))

            if rank >= 0:
                MRR = (1/rank)
            if rank >= 0 and rank <= P_AT:
                P_AT_X = 1.
            if rank == 1:
                P_AT_1 = 1.

    experiment_result["MRR"] = MRR
    experiment_result["P_AT_X"] = P_AT_X
    experiment_result["P_AT_1"] = P_AT_1
    experiment_result["PERPLEXITY"] = PERPLEXITY
    #
    # print("MRR: {}".format(experiment_result["MRR"]))
    # print("P_AT_X: {}".format(experiment_result["P_AT_X"]))
    # print("P_AT_1: {}".format(experiment_result["P_AT_1"]))
    # print("PERPLEXITY: {}".format(experiment_result["PERPLEXITY"]))

    return MRR, P_AT_X, experiment_result, return_msg
