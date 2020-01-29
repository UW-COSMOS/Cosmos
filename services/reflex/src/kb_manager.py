# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import pandas as pd
import numpy as np
from scipy.stats import norm
import pickle
from tqdm import tqdm
import os
import json
import spacy
from pprint import pprint
import pickle
import evaluation_metrics as metrics
import time, sys
import torch

MASK= "[MASK]"
ROBERTA_MASK="<mask>"
nlp = spacy.load("en_core_web_lg")

def default_accept(context, subject, template, label):
    return True, 0

def load_file(filename):
    data = []
    with open(filename, "r") as f:
        for line in f.readlines():
            data.append(json.loads(line))
    return data

def parse_template(template, subject_label, object_label):
    SUBJ_SYMBOL = "[X]"
    OBJ_SYMBOL = "[Y]"
    template = template.replace(SUBJ_SYMBOL, subject_label)
    template = template.replace(OBJ_SYMBOL, object_label)
    return [template]


def get_data_lists(data):
    samples_list = []
    sentences_list = []
    # sort to group togheter sentences with similar length
    for sample in sorted(
        data, key=lambda k: len(" ".join(k["masked_sentence"]).split())
    ):
        masked_sentence = sample["masked_sentence"]
        samples_list.append(sample)
        sentences_list.append(masked_sentence)
    return samples_list, sentences_list


def spacy_preprocess(text):
    doc = nlp(text)
    processed_list = [token.orth_ for token in doc if not(token.is_stop or token.is_space or token.is_stop)]
    return " ".join(processed_list)

def preprocess_data(samples):
    new_samples = []
    for sample in samples:
        sample["sub_label"] = sample["sub_label"].lower()
        lower_masked_sentences = []
        for sentence in sample["evidences"]:
            sentence = spacy_preprocess(sentence['masked_sentence'].lower())
            sentence = sentence.replace(MASK.lower(), MASK)
            lower_masked_sentences.append(sentence)
        sample["evidences"] = lower_masked_sentences
        new_samples.append(sample)
    return new_samples

def construct_samples(all_samples, template):
    facts = []
    sub_objs = []
    for sample in all_samples:
        sub = sample["sub_label"]
        target=None
        context=None
        if 'reconstructed_word' in sample:
           print("RECONSTRUCTED WORD")# should this ever be the case?  
            #raise Exception('Reconstructed word not in sample... fix this')
        else:
            if 'masked_sentence' in sample:
                print("Mask format")
                # Some of the masked sentences don't have a mask in them, need to find first with mask
              #  context = None
              #  for sent in sample['masked_sentence']:
              #      if '[MASK]'  in sent:
              #          context = sent.replace('[MASK]', sample['reconstructed_word'])
              #          break
              #  if context is None:
              #      print('No valid context found, skipping sample')
              #      continue
            else:
                context = None
                for evidence in sample['evidences']:
                    context = evidence
                if context is None:
                    print('No valid context found, skipping sample')
                    continue
        #TODO: is reconstructed word in sample? 
        if (sub, target, context) not in sub_objs:
            sub_objs.append((sub, target, context))
            if 'reconstructed_word' in sample:
                facts.append((sub, context, sample['reconstructed_word']))
            else:
                facts.append((sub, context, None))
    return facts_to_samples(facts, template)
   
def facts_to_samples(facts, template):
    all_samples = []
    for fact in facts:
        (sub, context, rw) = fact
        sample = {}
        sample["sub_label"] = sub
        sample["reconstructed_word"] = rw
        # substitute all sentences with a standard template
        sample['context'] = context
        sample["masked_sentence"] = parse_template(
            template.strip(), sample["sub_label"].strip(), MASK
        )
        all_samples.append(sample)
    return all_samples




def main(data_path, common_vocab_filename, max_sentence_length, template, relation_label, num_std_dev, model=None,  context_filter=None, single_token=False, inference_top_k=10):
    # deal with vocab subset
#    vocab_subset = None
#    index_list = None
#    if common_vocab_filename is not None: #TODO: is this given? 
#        vocab_subset = load_vocab(common_vocab_filename)
#
#        # optimization for some LM (such as ELMo)
#        model.optimize_top_layer(vocab_subset)
#
#      #  filter_logprob_indices, index_list = model.init_indices_for_filter_logprobs(
#      #     vocab_subset, logger
#      #  ) # TODO  Need this? 
#

    data = load_file(data_path)
    preprocessed_data = preprocess_data(data)

    all_samples = construct_samples(preprocessed_data, template)

   # create uuid if not present
    i = 0
    for sample in all_samples:
        if "uuid" not in sample:
            sample["uuid"] = i
            i += 1

    samples_list, sentences_list = get_data_lists(all_samples) 
    # Hyperparams for visualization 
    viz = True
    num_viz = 10
    final_viz = []
    viz_thres = 11

    sim_scores = []
    
    # Determine lower bound using context filter to measure similarity
    if context_filter is not None:
        for sample in samples_list:
            #print(sample)
            sim_score = context_filter.accept_context(sample['context'], sample['sub_label'], template.strip(), relation_label)
            sim_scores.append(sim_score)
        if len(sim_scores) > 0:
            sim_scores = np.asarray(sim_scores)
            mean, std = norm.fit(sim_scores)
            lower_bound = mean + num_std_dev * std
            print(f'Mean: {mean}, std: {std}, lower_bound: {lower_bound}')



    predictions_list = []
    anchor_list = []
    
    # Get predictions returned by voronoi_infer, keeping those whose similarity is larger than lower bound
    for sample in samples_list:

        accept_fn = default_accept if context_filter is None else context_filter.accept_context

        voronoi_probs, viz_seq, prediction1, prediction2 = model.voronoi_infer(sample['context'], template.strip(), sample['sub_label'], ROBERTA_MASK, k=inference_top_k)
        
        if single_token:
            prediction = prediction1
        else:
            prediction = prediction2

        if len(sim_scores) > 0:
            sim_score = context_filter.accept_context(sample['context'], sample['sub_label'], template.strip(), relation_label)
            if sim_score < lower_bound:
                prediction = ''
        if viz:
            if len(final_viz) != num_viz and i > viz_thres:
                final_viz.append(viz_seq)
        anchor_list.append(prediction1)
        predictions_list.append(prediction)

    torch.cuda.empty_cache()

#        original_log_probs_list, token_ids_list, masked_indices_list = model.get_batch_generation(
#            sentences_list, logger=logger # do we need the log_prob filtering? 
#        )
    if viz:
        with open('viz.pkl', 'wb') as wf:
            pickle.dump(final_viz, wf)
    return predictions_list
     #   if vocab_subset is not None:
     #       # filter log_probs
     #       filtered_log_probs_list = model.filter_logprobs(
     #           original_log_probs_list, filter_logprob_indices
     #       )
     #   else:
     #       filtered_log_probs_list = original_log_probs_list
     #   #REMOVE PAST HERE    


if __name__ == "__main__":
    print("Main") 
    #TODO: is this ever called from this file?
