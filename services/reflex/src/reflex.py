# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import time
import pandas as pd
import argparse
from kb_manager import main as base_evaluation
import statistics
from collections import defaultdict
from model import Roberta
from context_filtering.context_filtering import Context_Filtering
import sys
from config import *

def load_file(filename):
    data = []
    with open(filename, "r") as f:
        for line in f.readlines():
            data.append(json.loads(line))
    return data

#TODO: change path  with correct data format so no path passed in 
def run_experiments(relation_metadata, data_path, num_std_dev=num_std_dev, use_filter=use_filter):
    start_time = time.time()

    PARAMETERS = {
        "bert_vocab_name": "vocab.txt",
        "lm": "roberta",
        "label": "roberta_large",
        "models_names": ["roberta"],
        "roberta_model_name": "model.pt",
        "roberta_vocab_name": "dict.txt",
        "roberta_model_dir": "/data/roberta_large/",
    }
    model_args = argparse.Namespace(**PARAMETERS)

    model = Roberta(model_args)
    preds_res = []

    if use_filter:
        cf = Context_Filtering(we_model_pth, db_dump, word_weight_pth, N=num_random_contexts)
    else:
        cf = None

    template = relation_metadata["template"]
    relation_label  = relation_metadata["label"]
        
    pred_list = base_evaluation(data_path=data_path, common_vocab_filename=None, max_sentence_length=100, template=template, relation_label=relation_label, num_std_dev=num_std_dev, model=model, context_filter=cf, inference_top_k=num_top_k)
    print(pred_list)
    preds_res.append(pred_list)

    return preds_res

if __name__ == "__main__":
    data_path = sys.argv[1]
    relation_metadata = {"template":"[X] plays for [Y]", "label":"drafted by"}
    run_experiments(relation_metadata=relation_metadata, data_path=data_path)
    print("Finished running experiment")

    
