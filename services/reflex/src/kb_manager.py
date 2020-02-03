# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import numpy as np
from scipy.stats import norm
import pickle
import json
import spacy
import pickle
import time, sys
import torch

class KB_Manager:
    def __init__(self):
        self.MASK= "[self.MASK]"
        self.ROBERTA_MASK="<mask>"
        self.nlp = spacy.load("en_core_web_lg")
     
    def default_accept(self, context, subject, template, label):
        return True, 0
    
    def load_file(self, filename):
        data = []
        with open(filename, "r") as f:
            for line in f.readlines():
                data.append(json.loads(line))
        return data
    
    def parse_template(self, template, subject_label, object_label):
        SUBJ_SYMBOL = "[X]"
        OBJ_SYMBOL = "[Y]"
        template = template.replace(SUBJ_SYMBOL, subject_label)
        template = template.replace(OBJ_SYMBOL, object_label)
        return [template]
    
    def get_data_lists(self, data):
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
    
    
    def spacy_preprocess(self, text):
        doc = self.nlp(text)
        processed_list = [token.orth_ for token in doc if not(token.is_stop or token.is_space or token.is_stop)]
        return " ".join(processed_list)
    
    def preprocess_data(self, samples):
        new_samples = []
        for sample in samples:
            sample["sub_label"] = sample["sub_label"].lower()
            lower_masked_sentences = []
            for sentence in sample["evidences"]:
                sentence = self.spacy_preprocess(sentence['masked_sentence'].lower())
                sentence = sentence.replace(self.MASK.lower(), self.MASK)
                lower_masked_sentences.append(sentence)
            sample["evidences"] = lower_masked_sentences
            new_samples.append(sample)
        return new_samples
    
    def construct_samples(self, all_samples, template):
        facts = []
        sub_objs = []
        for sample in all_samples:
            sub = sample["sub_label"]
            target=None
            context=None
            for evidence in sample['evidences']:
               context = evidence
            if context is None:
                print('No valid context found, skipping sample')
                continue
            if (sub, target, context) not in sub_objs:
                sub_objs.append((sub, target, context))
                if 'reconstructed_word' in sample:
                    facts.append((sub, context, sample['reconstructed_word']))
                else:
                    facts.append((sub, context, None))
        return self.facts_to_samples(facts, template)
       
    def facts_to_samples(self,facts, template):
        all_samples = []
        for fact in facts:
            (sub, context, rw) = fact
            sample = {}
            sample["sub_label"] = sub
            sample["reconstructed_word"] = rw
            # substitute all sentences with a standard template
            sample['context'] = context
            sample["masked_sentence"] = self.parse_template(
                template.strip(), sample["sub_label"].strip(), self.MASK
            )
            all_samples.append(sample)
        return all_samples
    
    def get_predictions(self,data, common_vocab_filename, max_sentence_length, template, relation_label, num_std_dev, model=None,  context_filter=None, single_token=False, inference_top_k=10):
        #data = self.load_file(data_path)
        preprocessed_data = self.preprocess_data(data)
        all_samples = self.construct_samples(preprocessed_data, template)
       # create uuid if not present
        i = 0
        for sample in all_samples:
            if "uuid" not in sample:
                sample["uuid"] = i
                i += 1
        samples_list, sentences_list = self.get_data_lists(all_samples)
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
        # Get predictions returned by voronoi_infer, keeping those whose similarity is larger than lower bound
        for sample in samples_list:
            accept_fn = self.default_accept if context_filter is None else context_filter.accept_context
            voronoi_probs, viz_seq, single_pred, multi_pred = model.voronoi_infer(sample['context'], template.strip(), sample['sub_label'], self.ROBERTA_MASK, k=inference_top_k)
            if single_token:
                prediction = single_pred
            else:
                prediction = multi_pred
            if len(sim_scores) > 0:
                sim_score = context_filter.accept_context(sample['context'], sample['sub_label'], template.strip(), relation_label)
                if sim_score < lower_bound:
                    prediction = ''
            if viz:
                if len(final_viz) != num_viz and i > viz_thres:
                    final_viz.append(viz_seq)
            predictions_list.append(prediction)
        torch.cuda.empty_cache()
        if viz:
            with open('viz.pkl', 'wb') as wf:
                pickle.dump(final_viz, wf)
        return predictions_list
