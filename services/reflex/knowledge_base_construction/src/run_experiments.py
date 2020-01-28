# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import time
import pandas as pd
import argparse
from roberta_completion import main as base_evaluation
import pprint
import statistics
from collections import defaultdict
from roberta_connector import Roberta
from context_filtering.context_filtering import Context_Filtering
import sys
base_model = True
use_filter = False
zsre_analysis = False
MODEL_NAME = 'model.pt'
we_model_pth = '/data/cc.en.300.bin' 
db_dump = '/data/contexts.db'
word_weight_pth = '/data/wiki_word_weight.json'
num_random_contexts = 1000
exp_name = 'base_zsre'
num_top_k = 10
std_devs = [-2, -1, 0]



LMs = [
    # {
    #     "lm": "fairseq",
    #     "label": "fairseq",
    #     "models_names": ["fairseq"],
    #     "fairseq_model_name": 'wiki103.pt',
    #     "task": 'language_modeling',
    #     "cpu": True,
    #     "output_dictionary_size": -1,
    #     "data": "pre-trained_language_models/fairseq/wiki103_fconv_lm/"
    # },
    #{
    #    "lm":
    #    "transformerxl",
    #    "label":
    #    "transformerxl",
    #    "models_names": ["transformerxl"],
    #    "transformerxl_model_name":
    #    'transfo-xl-wt103',
    #    "transformerxl_model_dir":
    #    "pre-trained_language_models/transformerxl/transfo-xl-wt103/"
    #},
    #{
    #    "lm": "elmo",
    #    "label": "elmo",
    #    "models_names": ["elmo"],
    #    "elmo_model_name": 'elmo_2x4096_512_2048cnn_2xhighway',
    #    "elmo_vocab_name": 'vocab-2016-09-10.txt',
    #    "elmo_model_dir": "pre-trained_language_models/elmo/original",
    #    "elmo_warm_up_cycles": 10
    #},
    #    {
    #    "lm": "elmo",
    #    "label": "elmo5B",
    #    "models_names": ["elmo"],
    #    "elmo_model_name": "elmo_2x4096_512_2048cnn_2xhighway_5.5B",
    #    "elmo_vocab_name": "vocab-enwiki-news-500000.txt",
    #    "elmo_model_dir": "pre-trained_language_models/elmo/original5.5B/",
    #    "elmo_warm_up_cycles": 10
    #},
    #{
    #    "lm":
    #    "bert",
    #    "label":
    #    "bert_base",
    #    "models_names": ["bert"],
    #    "bert_model_name":
    #    "bert-base-cased",
    #    "bert_model_dir":
    #    "pre-trained_language_models/bert/cased_L-12_H-768_A-12"
    #},
    #{
    #    "lm": "bert",
    #    "label": "bert_large",
    #    "models_names": ["bert"],
    #    "bert_model_name": "bert-large-cased",
    #    "bert_model_dir": "pre-trained_language_models/bert/cased_L-24_H-1024_A-16",
    #}
    {
        "lm": "roberta",
        "label": "roberta_base",
        "models_names": ["roberta"],
        "roberta_model_name": MODEL_NAME,
        "roberta_vocab_name": "dict.txt",
        "roberta_model_dir": "/data",
    }
]

def load_file(filename):
    data = []
    with open(filename, "r") as f:
        for line in f.readlines():
            data.append(json.loads(line))
    return data


def run_experiments(relations,
    data_path,
    input_param={
        "lm": "bert",
        "label": "bert_large",
        "models_names": ["bert"],
        "bert_model_name": "bert-large-cased",
        "bert_model_dir": "pre-trained_language_models/bert/cased_L-24_H-1024_A-16",
    },
    num_std_dev = -1, use_filter=True
):
    start_time = time.time()
    model = None
    pp = pprint.PrettyPrinter(width=41, compact=True)

    all_Precision1 = []
    type_Precision1 = defaultdict(list)
    type_count = defaultdict(list)

    results_file = open("last_results.csv", "w+")
    PARAMETERS = {
        "bert_vocab_name": "vocab.txt",
        "lm": "roberta",
        "label": "roberta_large",
        "models_names": ["roberta"],
        "roberta_model_name": MODEL_NAME,
        "roberta_vocab_name": "dict.txt",
        "roberta_model_dir": "/data/roberta_large/",
    }
    model_args = argparse.Namespace(**PARAMETERS)

    model = Roberta(model_args)
    log_results = []
    error_results = []
    preds_res = []

    if use_filter:
        cf = Context_Filtering(we_model_pth, db_dump, word_weight_pth, N=num_random_contexts)
    else:
        cf = None

    total_num_samples = 0
    num_no_answer = 0
    num_single_token = 0
    num_multi_token = 0
    sum_lengths = 0
    for relation in relations:
        pp.pprint(relation)
        template = "X is the president of Y"
        relation_label="Abdul Hamid is the president of Bangladesh"
        if "template" in relation:
            template = relation["template"]
            relation_label  = relation["label"]
        else:
            print("NO TEMPLATE")

        pred_list = base_evaluation(data_path=data_path, common_vocab_filename=None, max_sentence_length=100, template=template, relation_label=relation_label, num_std_dev=num_std_dev, model=model, context_filter=cf, inference_top_k=num_top_k)
        print(pred_list)
        preds_res.append(pred_list)

        return preds_res

if __name__ == "__main__":
    data_path = sys.argv[1]
    relations = [{"template":"[X] plays for [Y]", "label":"drafted by"}]
    run_experiments(relations=relations, data_path=data_path)
    print("Finished running experiment")

    
