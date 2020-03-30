# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

from src.kb_manager import KB_Manager
from src.model import InferenceLM
from src.context_filtering.context_filtering import Context_Filtering
import sys
from src.config import *
from collections import Counter
from typing import List, Dict
model = InferenceLM()
manager = KB_Manager(use_filter, we_model_pth, db_dump, word_weight_pth, num_random_contexts)

def run_reflex(data:Dict[str, object], num_std_dev:int=num_std_dev) -> List[str]:
    """

    Run reflex model with config params and select result agreed on by most templates

    :param data: json object containing templates, samples, relation labels
    :param num_std_dev
    :return: list of string predictions

    """ 
    results = []
    for template in data['templates']:
        results.append(manager.get_predictions(data=data['samples'],  max_sentence_length=max_sentence_length, template=template, relation_label=data['label'], num_std_dev=num_std_dev, model=model, context_filter=manager.context_filter, inference_top_k=num_top_k))
    
    # Get the result agreed upon by majority of templates for each prediction
    pred_list = []
    for i in range(len(data['samples'])):
        temp_res = []
        for j in range(len(data['templates'])):
            temp_res.append(results[j][i])
        pred_list.append(max(temp_res, key=Counter(temp_res).get))

    return pred_list

