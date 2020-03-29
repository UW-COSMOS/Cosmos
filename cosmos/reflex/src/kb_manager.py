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
import logging
from typing import List, Tuple, Dict
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.WARNING)

class KB_Manager:
    def __init__(self, use_filter: bool, we_model_pth: str, db_dump: str, word_weight_pth: str, num_random_contexts: int) -> None:
        """

        Initialize with context filter and mask tokens

        """
        self.MASK= "[MASK]"
        self.ROBERTA_MASK="<mask>"
        self.nlp = spacy.load("en_core_web_lg")
        if use_filter:
            self.context_filter = Context_Filtering(we_model_pth, db_dump, word_weight_pth, N=num_random_contexts)
        else:
            self.context_filter = None

    def default_accept(self, context, subject, template, label):
        return True, 0
    
    def parse_template(self, template: str, subject_label: str, object_label: str) -> List[str]:
        """

        Substitute correct values into template

        :param samples: original template
        :param subject_label: subject in template
        :param object_label: object in template
        :return: processed template

        """ 
        SUBJ_SYMBOL = "[X]"
        OBJ_SYMBOL = "[Y]"
        template = template.replace(SUBJ_SYMBOL, subject_label)
        template = template.replace(OBJ_SYMBOL, object_label)
        return [template]
    
    def spacy_preprocess(self, text: str) -> str:
        """

        Remove stopwords, spaces, punctuation form text

        :param text
        :return: preprocessed text string

        """ 
        doc = self.nlp(text)
        processed_list = [token.orth_ for token in doc if not(token.is_stop or token.is_space or token.is_punct)]    
        return " ".join(processed_list)

    def preprocess_data(self, samples: Dict[str, object]) -> List[Dict[str, str]]:
        """

        Create list of preprocessed samples

        :param samples: original dict of samples
        :return: list of processed samples

        """ 
        new_samples = []
        for sample in samples:
            lower_masked_sentences = []
            for evidence in sample["evidences"]:
                sentence = evidence["masked_sentence"].lower()
                preprocessed = self.spacy_preprocess(sentence)
                masked = preprocessed.replace(self.MASK.lower(), self.MASK)
                lower_masked_sentences.append(masked)
            # make copy of samples to avoid editing original 
            new_samples.append(dict(sample))
            new_samples[len(new_samples)-1]['evidences'] = lower_masked_sentences
            new_samples[len(new_samples)-1]['sub_label'] = sample['sub_label'].lower()
        return new_samples
    
    def construct_samples(self, all_samples: Dict[str, object], template: str) -> Dict[str, str]:
        """

        Construct samples containing required metadata given list of original sample dictionaries

        :param all_samples: list of preprocessed sample dicts
        :template: processed template
        :return: list of new samples

        """ 
        facts = []
        sub_objs = []
        for sample in all_samples:
            sub = sample["sub_label"]
            target=None
            context=None
            for evidence in sample['evidences']:
               context = evidence
            if context is None:
                raise Exception('No valid context found, skipping sample')
                continue
            if (sub, target, context) not in sub_objs:
                sub_objs.append((sub, target, context))
                if 'target' in sample: # Can assume that target is always None
                    facts.append((sub, context, sample['target']))
                else:
                    facts.append((sub, context, None))
        return self.facts_to_samples(facts, template)
       
    def facts_to_samples(self,facts: List[Tuple[str, str, str]], template: str) -> Dict[str, str]:
        """

        Construct sample dictionaries from fact tuples

        :param facts: fact tuples
        :template: processed template
        :return: list of new samples

        """ 
        all_samples = []
        for fact in facts:
            (sub, context, target) = fact
            sample = {}
            sample["sub_label"] = sub
            sample["target"] = target
            # substitute all sentences with a standard template
            sample['context'] = context
            sample["masked_sentence"] = self.parse_template(
                template.strip(), sample["sub_label"].strip(), self.MASK
            )
            all_samples.append(sample)
        return all_samples
    
    def get_predictions(self, data: Dict[str, object], max_sentence_length: int, template: str, relation_label: str, num_std_dev: int, model: object,  context_filter: object = None, single_token: bool = False, inference_top_k: int = 10):
        """

        Invoke voronoi_infer model to get predictions for each relation in data and create visual

        :param data: list of dicts containing samples
        :param max_sentence_length
        :param template
        :param relation_label
        :param num_std_dev
        :param model: language model that infers relations 
        :param context_filter: improves results by selecting best contexts
        :template: processed template
        :single_token: prediction result
        :inference_top_k: num results for
        :return: list of string predictions

        """ 
        preprocessed_data = self.preprocess_data(data)
        samples_list = self.construct_samples(preprocessed_data, template)
        # Hyperparams for visualization 
        viz = True
        num_viz = 10
        final_viz = []
        viz_thres = 11
        sim_scores = []

        # Determine lower bound using context filter to measure similarity
        if context_filter is not None:
            for sample in samples_list:
                sim_score = context_filter.accept_context(sample['context'], sample['sub_label'], template.strip(), relation_label)
                sim_scores.append(sim_score)
            if len(sim_scores) > 0:
                sim_scores = np.asarray(sim_scores)
                mean, std = norm.fit(sim_scores)
                lower_bound = mean + num_std_dev * std
                logging.info(f'Mean: {mean}, std: {std}, lower_bound: {lower_bound}')
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
                if len(final_viz) != num_viz and len(samples_list) > viz_thres:
                    final_viz.append(viz_seq)
            predictions_list.append(prediction)
        if viz:
            with open('viz.pkl', 'wb') as wf:
                pickle.dump(final_viz, wf)
        return predictions_list
