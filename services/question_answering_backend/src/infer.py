import os
import sys
import glob
import torch
import uuid
import json

module_path = os.path.abspath(os.path.join('pytorch-transformers'))
if module_path not in sys.path:
    sys.path.append(module_path)

module_path = os.path.abspath(os.path.join('pytorch-transformers/examples'))
if module_path not in sys.path:
    sys.path.append(module_path)

from pytorch_transformers.tokenization_bert import BasicTokenizer, whitespace_tokenize
from examples.utils_squad import *

from pytorch_transformers import (BertConfig, BertForQuestionAnswering, BertTokenizer)
from torch.utils.data import TensorDataset, DataLoader


model_path = "models/bert_base_uncased_finetuned_squad_2.0/"
#Initialising parameters
max_query_length = 60
max_seq_length = 384
doc_stride = 128
device = torch.device("cuda")
version_2_with_negative = False
n_best_size = 20
max_answer_length = 30
#Loading the model
model_name_or_path = "bert-base-uncased"
do_lower_case=True
config_class, model_class, tokenizer_class = (BertConfig,
			      BertForQuestionAnswering, BertTokenizer)
tokenizer = tokenizer_class.from_pretrained(model_name_or_path, do_lower_case=True)
model = model_class.from_pretrained(model_path)
model.to(device)


def convert_to_squad_example(question, paragraph):
    return {'data':[{'title': 'custom_question', 'paragraphs': [{'context': paragraph, 'qas': [{'answers': [], 'question': question, 'id': str(uuid.uuid4())}]}]}]}

def load_example(question, paragraph):
    squad_examples = convert_to_squad_example(question, paragraph)
    with open('temp.json', 'w') as outfile:
        json.dump(squad_examples, outfile)
    formatted_examples = read_squad_examples(input_file='temp.json', 
                                   is_training=False,
                                   version_2_with_negative=False)
    features = convert_examples_to_features(examples=formatted_examples,
                                            tokenizer=tokenizer,
                                            max_seq_length=max_seq_length,
                                            doc_stride=doc_stride,
                                            max_query_length=max_query_length,
                                            is_training=False)
    all_input_ids = torch.tensor([f.input_ids for f in features], dtype=torch.long)
    all_input_mask = torch.tensor([f.input_mask for f in features], dtype=torch.long)
    all_segment_ids = torch.tensor([f.segment_ids for f in features], dtype=torch.long)
    all_cls_index = torch.tensor([f.cls_index for f in features], dtype=torch.long)
    all_p_mask = torch.tensor([f.p_mask for f in features], dtype=torch.float)
    all_example_index = torch.arange(all_input_ids.size(0), dtype=torch.long)
    
    dataset = (all_input_ids, all_input_mask, all_segment_ids,
                                all_example_index, all_cls_index, all_p_mask)

    return dataset, formatted_examples, features

def infer_qa(question, paragraph):
    dataset, examples, features = load_example(question, paragraph)
    #dataloader = DataLoader(dataset)

    #batch = iter(dataloader).next()
    input_ids, input_mask, segment_ids, start_positions, end_positions, example_index = dataset
    model.eval()
    input_ids = input_ids.to(device)
    input_mask = input_mask.to(device)
    segment_ids = segment_ids.to(device)
    start_positions = start_positions.to(device)
    end_positions = end_positions.to(device)

    batch_start_logits, batch_end_logits = model(input_ids, segment_ids, input_mask)

    start_logits = batch_start_logits[0].detach().cpu().tolist()
    end_logits = batch_end_logits[0].detach().cpu().tolist()
    eval_feature = features[0]
    unique_id = int(eval_feature.unique_id)
    all_results =[]
    all_results.append(RawResult(unique_id=unique_id,
                                 start_logits=start_logits,
                                 end_logits=end_logits))

    return write_predictions(
        examples,
        features,
        all_results,
        n_best_size,
        max_answer_length,
        do_lower_case,
        "temp1",
        "temp2",
        "temp3",
        False,
        version_2_with_negative,
        0.0)
