import os
import sys
import glob
import torch
import uuid
import json
import logging

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


model_path = "../../../deployment/weights/bert_base_squad2/"
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

verbose_logging = False

output_prediction_file = "prediction"
output_nbest_file = "nbest"

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
    all_example_index = torch.arange(all_input_ids.size(0), dtype=torch.long)
    
    dataset = (all_input_ids, all_input_mask, all_segment_ids,
                                all_example_index)

    return dataset, formatted_examples, features

def infer_qa(question, paragraph):
    dataset, examples, features = load_example(question, paragraph)
    #dataloader = DataLoader(dataset)

    #batch = iter(dataloader).next()
    all_input_ids, all_input_mask, all_segment_ids, all_example_index = dataset
    model.eval()
    all_input_ids = all_input_ids.to(device)
    all_input_mask = all_input_mask.to(device)
    all_segment_ids = all_segment_ids.to(device)

    inputs = {'input_ids': all_input_ids, 'attention_mask' : all_input_mask, 'token_type_ids': all_segment_ids}
    outputs = model(**inputs)
    
    all_results = []
    for i, example_index in enumerate(all_example_index):
        feature = features[example_index.item()]
        unique_id = int(feature.unique_id)
        all_results.append(RawResult(unique_id=unique_id,
                                     start_logits=outputs[0][i].detach().cpu().tolist(),
                                     end_logits=outputs[1][i].detach().cpu().tolist()))

    predictions = write_predictions(
        examples,
        features,
        all_results,
        n_best_size,
        max_answer_length,
        do_lower_case,
        output_prediction_file,
        output_nbest_file,
        "temp3",
        verbose_logging,
        version_2_with_negative,
        0.0)
    
    with open(output_nbest_file, "r") as reader:
        probabilities = next(iter(json.load(reader).values()))[0]['probability']

    logging.info(predictions)
    
    return list(predictions.values())[0], probabilities
