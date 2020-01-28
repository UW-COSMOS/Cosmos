# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import time
import pandas as pd
#from drqa import retriever
import argparse
#from batch_eval_KB_completion import main as run_evaluation
#from context_retrieval_completion import main as run_context_evaluation, run_analysis
from roberta_completion import main as base_evaluation
#from context_qa_completion import main as run_qa_evaluation
from batch_eval_KB_completion import load_file
from lama.modules import build_model_by_name
import pprint
import statistics
from os import listdir
import os
from os.path import isfile, join
from shutil import copyfile
from collections import defaultdict
from roberta_connector import Roberta
import sqlite3
#from transformers import BertConfig, BertForQuestionAnswering, BertTokenizer
from context_filtering.context_filtering import Context_Filtering
import sys
base_model = True
#qa = False
#v2 = False
is_zsre = True
#single_token = False
#must_choose_answer = False
use_filter = False
zsre_analysis = False
#RELATIONS_F = 'relations'
#RELATIONS_F = 'zsre_relations'
RELATIONS_F = 'zsre_relations'
#qa_path = '/data/squad2'
MODEL_NAME = 'model.pt'
#ENCODER_LAYERS = 12
#ENCODER_EMBED_DIM = 768
#ENCODER_FFN_EMBED_DIM = 3072
#ENCODER_ATTENTION_HEADS = 12
we_model_pth = '/data/cc.en.300.bin' 
db_dump = '/data/contexts.db'
word_weight_pth = '/data/wiki_word_weight.json'
num_random_contexts = 1000
exp_name = 'base_zsre'
num_top_k = 10
std_devs = [-2, -1, 0]

# Conditions error analysis

#condition_on_answer_exists = False
#condition_on_single_token = False
#condition_on_multi_token = False
#condition_on_answer_does_not_exist = False
#
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
    #if qa:
    #    tokenizer = BertTokenizer.from_pretrained('bert-large-cased')
    #    qamodel = BertForQuestionAnswering.from_pretrained(qa_path)
    #    qamodel.eval()

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
#       PARAMETERS = {
#            "dataset_filename": "{}{}{}".format(
#                data_path_pre, relation["relation"], data_path_post
#            ),
#            "common_vocab_filename": None,#"pre-trained_language_models/common_vocab_cased.txt",
#            "template": "",
#            "bert_vocab_name": "vocab.txt",
#            "batch_size": 32,
#            "logdir": "output",
#            "full_logdir": "output/results/{}/{}".format(
#                input_param["label"], relation["relation"]
#            ),
#            "lowercase": False,
#            "max_sentence_length": 100,
#            "threads": -1,
#            "interactive": False,
#            "num_std_dev": num_std_dev 
#        }
        template = "X is the president of Y"
        relation_label="Abdul Hamid is the president of Bangladesh"
        if "template" in relation:
            template = relation["template"]
            print(template)
            #PARAMETERS["template"] = relation["template"]
            #if qa and len(relation['template']) == 0:
            #    PARAMETERS["template"] = '[X] is a [Y]'
            #PARAMETERS["question"] = relation["question"]
            relation_label  = relation["label"]
            print(relation_label)
        else:
            print("NO TEMPLATE")

#        PARAMETERS.update(input_param)
        
#        args = argparse.Namespace(**PARAMETERS)
        # see if file exists
        #try:
        #    data = load_file(data_path)
        #except Exception as e:
        #    print("Relation {} excluded.".format(relation["relation"]))
        #    print("Exception: {}".format(e))
        #    continue


#        if qa:
#            Precision1, Precision10, MRR, EM, F1, is_error, no_overlap, larger_by_1, larger_by_2, larger_by_3, larger_by_4, larger_by_5_or_more= run_qa_evaluation(args, shuffle_data=False, model=model, qamodel=qamodel, tokenizer=tokenizer, zsre=is_zsre, v2=v2, must_choose_answer=must_choose_answer, condition_on_answer_exists=condition_on_answer_exists,
#                                                                        condition_on_single_token=condition_on_single_token,
#                                                                        condition_on_multi_token=condition_on_multi_token,
#                                                                        condition_on_answer_does_not_exist=condition_on_answer_does_not_exist)
#
#            error_results.append((relation['label'], is_error, no_overlap, larger_by_1, larger_by_2, larger_by_3, larger_by_4, larger_by_5_or_more))
#            if F1 is None:
#                print(f'This relation contains no examples for the specified conditions: {relation["label"]}')
#                continue
#            log_results.append((relation['label'], EM, F1))
#        elif base_model:
#            Precision1, Precision10, MRR, EM, F1 = run_context_evaluation(args, shuffle_data=False, model=model, zsre=is_zsre, context_filter=cf, single_token=single_token, inference_top_k=num_top_k)
#            if F1 is None:
#                continue
#            log_results.append((relation['label'], EM, F1))
#
#       if zsre_analysis: #TODO: modify the arguments in analysis
#            r_num_samples, r_no_answer, r_single_token, r_multi_token, r_sum_lengths = run_analysis(data_path, template, bert_vocab_name="vocab.txt", logdir= "output/results/{}/{}".format(input_param["label"], relation["relation"]), max_sentence_length=100, interactive=False, num_std_dev=-1, model=model)
#            #r_num_samples, r_no_answer, r_single_token, r_multi_token, r_sum_lengths = run_analysis(args, model)
#            total_num_samples += r_num_samples
#            num_no_answer += r_no_answer
#            num_single_token += r_single_token
#            num_multi_token += r_multi_token
#            sum_lengths += r_sum_lengths
#        else:
        pred_list = base_evaluation(data_path=data_path, common_vocab_filename=None, max_sentence_length=100, template=template, relation_label=relation_label, num_std_dev=num_std_dev, model=model, context_filter=cf, inference_top_k=num_top_k)
        print(pred_list)
        preds_res.append(pred_list)

      #  if F1 is None:
      #      continue
      #  log_results.append((relation['label'], EM, F1))
      #  error_results.append((relation['label'], is_error, pred_too_large, pred_too_small, should_be_empty, should_be_not_empty, anchor_outside, mismatch))
      #  if not zsre_analysis:
#            print("P@1 : {}".format(Precision1), flush=True)
#            all_Precision1.append(Precision1)
#
#            results_file.write(
#                "{},{},{},{},{}\n".format(relation["relation"], round(Precision1 * 100, 2), round(Precision10 * 100, 2), round(MRR * 100, 2), relation)
#            )
#            results_file.flush()
#
#            if "type" in relation:
#                type_Precision1[relation["type"]].append(Precision1)
#                data = load_file(PARAMETERS["dataset_filename"])
#                type_count[relation["type"]].append(len(data))
#
    return preds_res

#    if zsre_analysis:
#        mean_length = sum_lengths / (num_single_token + num_multi_token)
#        with open('zsre_characteristics.csv', 'w') as wf:
#            wf.write(f'Total Number of Samples, Number Single Token, Number Multi Token, Number no answer, Mean length\n')
#            wf.write(f'{total_num_samples},{num_single_token},{num_multi_token},{num_no_answer},{mean_length}\n')
#        return
#
#    end_time = time.time()
#    with open(f'{exp_name}_{num_std_dev}_time.txt', 'w') as wf:
#        wf.write(f'{end_time - start_time} seconds\n')
#
#    mean_p1 = statistics.mean(all_Precision1)
#    print("@@@ {} - mean P@1: {}".format(input_param["label"], mean_p1))
#    results_file.close()
#    logdf = pd.DataFrame(log_results, columns=['relation', 'EM', 'F1'])
#    logdf.to_csv(f'{exp_name}_{num_std_dev}.csv')
#    if len(error_results) > 0:
##        if not qa:
##            errordf = pd.DataFrame(error_results, columns=['relation', 'is_error', 'pred_too_large', 'pred_too_small', 'should_be_empty', 'should_be_not_empty', 'anchor_outside', 'mismatch'])
##        else:
#        errordf = pd.DataFrame(error_results, columns=['relation', 'is_error', 'no_overlap', 'larger_by_1', 'larger_by_2', 'larger_by_3', 'larger_by_4', 'larger_by_5_or_more'])
#        errordf.to_csv(f'{exp_name}_{num_std_dev}_error.csv')
#
#    for t, l in type_Precision1.items():
#
#        print(
#            "@@@ ",
#            input_param["label"],
#            t,
#            statistics.mean(l),
#            sum(type_count[t]),
#            len(type_count[t]),
#            flush=True,
#        )
#
#    return mean_p1, all_Precision1
#

#def get_TREx_parameters(data_path_pre="/data/"):
#    relations = load_file(f"{data_path_pre}{RELATIONS_F}.jsonl")
#    data_path_pre += "TREx/"
#    data_path_post = ".jsonl"
#    return relations, data_path_pre, data_path_post
#
#def get_zsre_parameters(data_path_pre="/data/"):
#    relations = load_file(f"{data_path_pre}{RELATIONS_F}.jsonl")
#    data_path_pre += "zsre_trex/"
#    data_path_post = ".jsonl"
#    return relations, data_path_pre, data_path_post
#

#def get_GoogleRE_parameters():
#    relations = [
#            {"relation": "place_of_birth", "template": "[X] was born in the city of [Y]", "question": "Where was [X] born?", "label": "Place of birth"},
#            {"relation": "date_of_birth", "template": "[X] was born in the year [Y]", "question": "When was [X] born?", "label": "Date of birth"},
#            {"relation": "place_of_death", "template": "[X] died in the city of [Y]", "question": "Where did [X] die?", "label": "Place of death"},
#    ]
#    data_path_pre = "/data/Google_RE/"
#    data_path_post = "_test.jsonl"
#    return relations, data_path_pre, data_path_post
#
#
#def get_ConceptNet_parameters(data_path_pre="/data/"):
#    relations = [{"relation": "test"}]
#    data_path_pre += "ConceptNet/"
#    data_path_post = ".jsonl"
#    return relations, data_path_pre, data_path_post
#

#def get_Squad_parameters(data_path_pre="/data/"):
#    relations = [{"relation": "test"}]
#    data_path_pre += "Squad/"
#    data_path_post = ".jsonl"
#    return relations, data_path_pre, data_path_post
#
#
#def run_all_LMs(parameters):
#    for ip in LMs:
#        print(ip["label"])
#        run_experiments(*parameters, input_param=ip)
#
#
#def run_loop(parameters):
#    for val in std_devs:
#        for ip in LMs:
#            print(ip["label"])
#            run_experiments(*parameters, input_param=ip, num_std_dev=val)
#
if __name__ == "__main__":

    #print("1. Google-RE")    
    #parameters = get_GoogleRE_parameters()
    #run_all_LMs(parameters)

    #print("2. T-REx")    
    #parameters = get_TREx_parameters()
    #run_all_LMs(parameters)

    #print("3. ConceptNet")
    #parameters = get_ConceptNet_parameters()
    #run_all_LMs(parameters)

    #print("4. SQuAD")
    #parameters = get_Squad_parameters()
    #run_all_LMs(parameters)
    #print("5. ZSRE")    
    #parameters = get_zsre_parameters()
    #run_all_LMs(parameters)
    #run_loop(parameters)
    data_path = sys.argv[1]
    relations = [{"template":"[X] plays for [Y]", "label":"drafted by"}]
    run_experiments(relations=relations, data_path=data_path)
    print("Finished running experiment")
    #print(f'Finished running experiment: {exp_name}')

    
