# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import time
from src.kb_manager import KB_Manager
from src.model import Roberta
from src.context_filtering.context_filtering import Context_Filtering
import sys
from src.config import *

def run_reflex(relation_metadata, data, num_std_dev=num_std_dev, use_filter=use_filter):
    start_time = time.time()

    model = Roberta()
    preds_res = []

    if use_filter:
        cf = Context_Filtering(we_model_pth, db_dump, word_weight_pth, N=num_random_contexts)
    else:
        cf = None

    template = relation_metadata["template"]
    relation_label  = relation_metadata["label"]

    manager = KB_Manager()
    pred_list = manager.get_predictions(data=data, common_vocab_filename=None, max_sentence_length=max_sentence_length, template=template, relation_label=relation_label, num_std_dev=num_std_dev, model=model, context_filter=cf, inference_top_k=num_top_k)
    print(pred_list)
    preds_res.append(pred_list)

    return preds_res

if __name__ == "__main__":
    #data = sys.argv[1]
    #relation_metadata = sys.argv[2]
    data = 'relation.jsonl'
    relation_metadata = {"template":"[X] plays for [Y]", "label":"drafted by"}
    run_experiments(relation_metadata=relation_metadata, data=data)
    print("Finished running experiment")

    
