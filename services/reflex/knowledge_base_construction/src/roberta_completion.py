# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import pandas as pd
import numpy as np
from scipy.stats import norm
#from retrieve_docs import retrieve_docs
#from lama.modules import build_model_by_name
import pickle
#import lama.utils as utils
#from lama.utils import  load_vocab TODO: check data format if vocab subset given, include this
#import lama.options as options
from tqdm import tqdm
import os
import json
import spacy
#import lama.modules.base_connector as base 
from pprint import pprint
#import logging.config
#import logging
import pickle
#from multiprocessing.pool import ThreadPool
#import multiprocessing
import evaluation_metrics as metrics
import time, sys
import torch
#import gc

#logger = logging.getLogger()
#logger.setLevel(logging.INFO)
#fmt = logging.Formatter('%(asctime)s: [ %(message)s ]', '%m/%d/%Y %I:%M:%S %p')
#console = logging.StreamHandler()
#console.setFormatter(fmt)
#logger.addHandler(console)

MASK= "[MASK]"
ROBERTA_MASK="<mask>"
nlp = spacy.load("en_core_web_sm")
import run

def default_accept(context, subject, template, label):
    return True, 0

def load_file(filename):
    data = []
    with open(filename, "r") as f:
        for line in f.readlines():
            data.append(json.loads(line))
    return data


#$def create_logdir_with_timestamp(base_logdir, modelname):
#$    timestr = time.strftime("%Y%m%d_%H%M%S")
#$
#$    # create new directory
#$    log_directory = "{}/{}_{}/".format(base_logdir, modelname, timestr)
#$    os.makedirs(log_directory)
#$
#$    path = "{}/last".format(base_logdir)
#$    try:
#$        os.unlink(path)
#$    except Exception:
#$        pass
#$    os.symlink(log_directory, path)
#$    return log_directory
#$

def parse_template(template, subject_label, object_label):
    SUBJ_SYMBOL = "[X]"
    OBJ_SYMBOL = "[Y]"
    template = template.replace(SUBJ_SYMBOL, subject_label)
    template = template.replace(OBJ_SYMBOL, object_label)
    return [template]


#def init_logging(log_directory):
#    logger = logging.getLogger("LAMA")
#    logger.setLevel(logging.DEBUG)
#
#    os.makedirs(log_directory, exist_ok=True)
#
#    # logging format
#    # "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
#    formatter = logging.Formatter(
#        "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
#    )
#
#    # file handler
#    fh = logging.FileHandler(str(log_directory) + "/info.log")
#    fh.setLevel(logging.DEBUG)
#    fh.setFormatter(formatter)
#
#    # console handler
#    ch = logging.StreamHandler(sys.stdout)
#    ch.setLevel(logging.WARNING)
#    ch.setFormatter(formatter)
#
#    logger.addHandler(fh)
#    logger.addHandler(ch)
#
#    logger.propagate = False
#
#    return logger
#

def get_data_lists(data):
    samples_list = []
    sentences_list = []
    #current_samples_batch = []
    #current_sentences_batches = []
    #c = 0

    # sort to group togheter sentences with similar length
    for sample in sorted(
        data, key=lambda k: len(" ".join(k["masked_sentence"]).split())
    ):
        masked_sentence = sample["masked_sentence"]
        samples_list.append(sample)
        sentences_list.append(masked_sentence)
    return samples_list, sentences_list


#def run_thread(arguments):
#
#    msg = ""
#
#    # 1. compute the ranking metrics on the filtered log_probs tensor
#    sample_MRR, sample_P, experiment_result, return_msg = metrics.get_ranking(
#        arguments["mymodel_probs"],
#        arguments["masked_indices"],
#        arguments["vocab"],
#        is_masked_probs=True,
#        label_index=arguments["label_index"],
#        print_generation=arguments["interactive"],
#        topk=10000,
#    )
#    em, f1, is_error, pred_too_large, pred_too_small, should_be_empty, should_be_not_empty, anchor_outside, mismatch = metrics.calculate_em_f1(arguments['target'], arguments['prediction'], arguments['anchor'])
#    msg += "\n" + return_msg
#
#    sample_perplexity = 0.0
#    if arguments["interactive"]:
#        pprint(arguments["sample"])
#        # THIS IS OPTIONAL - mainly used for debuggind reason
#        # 2. compute perplexity and print predictions for the complete log_probs tensor
#        sample_perplexity, return_msg = print_sentence_predictions(
#            arguments["original_log_probs"],
#            arguments["token_ids"],
#            arguments["vocab"],
#            masked_indices=arguments["masked_indices"],
#            print_generation=arguments["interactive"],
#        )
#        input("press enter to continue...")
#        msg += "\n" + return_msg
#
#    return experiment_result, sample_MRR, sample_P, sample_perplexity, msg, em, f1, is_error, pred_too_large, pred_too_small, should_be_empty, should_be_not_empty, anchor_outside, mismatch
#
def spacy_preprocess(text):
    doc = nlp(text)
    processed_list = [token.orth_ for token in doc if not(token.is_stop or token.is_space or token.is_stop)]
    return " ".join(processed_list)

def preprocess_samples(samples):
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


#def run_analysis(args, model, zsre=True):
#
#
#    # stats
#    samples_with_negative_judgement = 0
#    samples_with_positive_judgement = 0
#
#
#    data = load_file(args.dataset_filename)
#
#    data_len = len(data)
#    print(len(data))
#
#    if args.lowercase:
#        # lowercase all samples
#        logger.info("lowercasing all samples...")
#        all_samples = lowercase_samples(data)
#    else:
#        # keep samples as they are
#        all_samples = data
#
#    vocab_subset = None
#    all_samples, ret_msg = filter_samples(
#        model, data, vocab_subset, args.max_sentence_length, args.template, is_zsre=zsre
#    )
#
#    # OUT_FILENAME = "{}.jsonl".format(args.dataset_filename)
#    # with open(OUT_FILENAME, 'w') as outfile:
#    #     for entry in all_samples:
#    #         json.dump(entry, outfile)
#    #         outfile.write('\n')
#
#    logger.info("\n" + ret_msg + "\n")
#
#    print(len(all_samples))
#
#    # if template is active (1) use a single example for (sub,obj) and (2) ...
#    if args.template and args.template != "":
#        facts = []
#        sub_objs = []
#        for sample in all_samples:
#            sub = sample["sub_label"]
#            obj = sample["obj_label"]
#            target = sample['reconstructed_word']
#            if 'reconstructed_word' not in sample:
#                raise Exception('Reconstructed word not in sample... fix this')
#            else:
#                if 'masked_sentences' in sample:
#                    # Some of the masked sentences don't have a mask in them, need to find first with mask
#                    context = None
#                    for sent in sample['masked_sentences']:
#                        if '[MASK]'  in sent:
#                            context = sent.replace('[MASK]', sample['reconstructed_word'])
#                            break
#                    if context is None:
#                        print('No valid context found, skipping sample')
#                        continue
#                else:
#                    context = None
#                    for evidence in sample['evidences']:
#                        if not zsre:
#                            if '[MASK]' in evidence['masked_sentence']:
#                                context = evidence['masked_sentence'].replace('[MASK]', sample['reconstructed_word'])
#                                break
#                        else:
#                            context = evidence['masked_sentence']
#                    if context is None:
#                        print('No valid context found, skipping sample')
#                        continue
#
#            #context = context.replace('(', '')
#            #context = context.replace(')', '')
#            
#            if (sub, target, context) not in sub_objs:
#                sub_objs.append((sub, target, context))
#                if 'reconstructed_word' in sample:
#                    facts.append((sub, obj, context, sample['reconstructed_word']))
#                else:
#                    facts.append((sub, obj, context, obj))
#            else:
#                excluded_count += 1 
#
#                #break
#        local_msg = "distinct template facts: {}".format(len(facts))
#        logger.info("\n" + local_msg + "\n")
#        print(local_msg)
#        all_samples = []
#        for fact in facts:
#            (sub, obj, context, rw) = fact
#            sample = {}
#            sample["sub_label"] = sub
#            sample["obj_label"] = obj
#            sample["reconstructed_word"] = rw
#            # sobstitute all sentences with a standard template
#            sample['context'] = context
#            sample["masked_sentences"] = parse_template(
#                args.template.strip(), sample["sub_label"].strip(), base.MASK
#            )
#            #query = sample['masked_sentences'][0].replace(base.MASK, '')
#            #sample['query'] = query
#            #print(f'query={query}')
#            #docs = retrieve_docs(query, ranker, conn, 30)
#            #sample['context'] = docs[0]
#            #print(f'docs={docs}')
#            all_samples.append(sample)
#    #else:
#    #    for sample in all_samples:
#    #        query = sample['masked_sentences'][0].replace(base.MASK, '')
#    #        sample['query'] = query
#    #        #print(f'query={query}')
#    #        docs = retrieve_docs(query, ranker, conn, 1)
#    #        sample['context'] = docs[0]
#            
#
#    # create uuid if not present
#    i = 0
#    for sample in all_samples:
#        if "uuid" not in sample:
#            sample["uuid"] = i
#        i += 1
#
#    total_num_samples = len(all_samples)
#    num_no_answer = 0
#    num_single_token = 0
#    num_multi_token = 0
#    sum_lengths = 0
#
#    for sample in all_samples:
#        if sample['reconstructed_word'] == '':
#            num_no_answer += 1
#        else:
#            spl = sample['reconstructed_word'].split(' ')
#            spl = [s for s in spl if s != '']
#            sum_lengths += len(spl)
#            if len(spl) == 1:
#                num_single_token += 1
#            else:
#                num_multi_token += 1
#    return total_num_samples, num_no_answer, num_single_token, num_multi_token, sum_lengths, data_len
#
            

def main(data_path, common_vocab_filename, max_sentence_length, template, relation_label, num_std_dev, model=None,  context_filter=None, single_token=False, inference_top_k=10):
    msg = ""
    # deal with vocab subset
    vocab_subset = None
    index_list = None
    if common_vocab_filename is not None: #TODO: is this given? 
        vocab_subset = load_vocab(common_vocab_filename)
        msg += "common vocabulary size: {}\n".format(len(vocab_subset))

        # optimization for some LM (such as ELMo)
        model.optimize_top_layer(vocab_subset)

      #  filter_logprob_indices, index_list = model.init_indices_for_filter_logprobs(
      #     vocab_subset, logger
      #  ) # Need this? 


    # stats #TODO: Do we need these at all?
    samples_with_negative_judgement = 0
    samples_with_positive_judgement = 0

    # Mean reciprocal rank
    MRR = 0.0
    MRR_negative = 0.0
    MRR_positive = 0.0

    # Precision at (default 10)
    Precision = 0.0
    Precision1 = 0.0
    Precision_negative = 0.0
    Precision_positivie = 0.0

    # EM
    EM = 0.0

    # F1
    F1 = 0.0
    is_error = 0
    pred_too_large = 0
    pred_too_small = 0
    should_be_empty = 0
    should_be_not_empty = 0
    anchor_outside = 0
    mismatch = 0
    data = load_file(data_path)

    #print(len(data))
     #TODO preprocessing here
     
    all_samples = preprocess_samples(data)

    #all_samples, ret_msg = filter_samples(
    #    model, data, vocab_subset, max_sentence_length, template, is_zsre=True
    #)

    # OUT_FILENAME = "{}.jsonl".format(args.dataset_filename)
    # with open(OUT_FILENAME, 'w') as outfile:
    #     for entry in all_samples:
    #         json.dump(entry, outfile)
    #         outfile.write('\n')

    #logger.info("\n" + ret_msg + "\n")

    #TODO: make def construct_facts
    facts = []
    sub_objs = []
    for sample in all_samples:
     #   print(sample)
        sub = sample["sub_label"]
        #target = sample['reconstructed_word']
        target=None
        context="bad context"
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
      #  print("context")
       # print(context)
        #TODO: is reconstructed word in sample? 
        if (sub, target, context) not in sub_objs:
            sub_objs.append((sub, target, context))
            if 'reconstructed_word' in sample:
                facts.append((sub, context, sample['reconstructed_word']))
            else:
                facts.append((sub, context, None))

    local_msg = "distinct template facts: {}".format(len(facts))
    #logger.info("\n" + local_msg + "\n")
    #print(local_msg)
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

   # create uuid if not present
    i = 0
    for sample in all_samples:
        if "uuid" not in sample:
            sample["uuid"] = i
            i += 1

    samples_list, sentences_list = get_data_lists(all_samples) #TODO: should we leave batching as an option? 

    viz = True
    num_viz = 10
    final_viz = []
    viz_thres = 11

    sim_scores = []

    if context_filter is not None:
        for sample in samples_list:
           # print('sample')
            print(sample)
           # print(sample['context'])
           # print(sample['sub_label'])
           # print(template.strip())
            sim_score = context_filter.accept_context(sample['context'], sample['sub_label'], template.strip(), relation_label)
            sim_scores.append(sim_score)
        if len(sim_scores) > 0:
            sim_scores = np.asarray(sim_scores)
            mean, std = norm.fit(sim_scores)
            lower_bound = mean + num_std_dev * std
            print(f'Mean: {mean}, std: {std}, lower_bound: {lower_bound}')



    predictions_list = []
    anchor_list = []
    
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

#        label_index_list = []
#        for sample in samples_b:
#            obj_label_id = model.get_id(sample["obj_label"])
#
#            # MAKE SURE THAT obj_label IS IN VOCABULARIES
#            if obj_label_id is None:
#                raise ValueError(
#                    "object label {} not in model vocabulary".format(
#                        sample["obj_label"]
#                    )
#                )
#            #elif model.vocab[obj_label_id[0]] != sample["obj_label"]:
#            #    raise ValueError(
#            #        "object label {} not in model vocabulary".format(
#            #            sample["obj_label"]
#            #        )
#            #    )
#            elif vocab_subset is not None and sample["obj_label"] not in vocab_subset:
#                raise ValueError(
#                    "object label {} not in vocab subset".format(sample["obj_label"])
#                )
#
#            label_index_list.append(obj_label_id)
#
#        arguments = [
#            {
#                "mymodel_probs" mymodel_probs,
#                "original_log_probs": original_log_probs,
#                "filtered_log_probs": filtered_log_probs,
#                "target": sample["reconstructed_word"],
#                "prediction": pred,
#                "token_ids": token_ids,
#                "vocab": model.vocab,
#                "label_index": label_index[0] if len(label_index) > 0 else 0,
#                "masked_indices": masked_indices,
#                "interactive": args.interactive,
#                "index_list": index_list,
#                "sample": sample,
#                "anchor": anchor
#            }
#            for mymodel_probs, original_log_probs, filtered_log_probs, token_ids, masked_indices, label_index, sample, pred, anchor in zip(
#                mymodel_probs_list,
#                original_log_probs_list,
#                filtered_log_probs_list,
#                token_ids_list,
#                masked_indices_list,
#                label_index_list,
#                samples_b,
#                predictions_list,
#                anchor_list
#            )
#        ]
#
#        # single thread for debug
#        # for isx,a in enumerate(arguments):
#        #     print(samples_b[isx])
#        #     run_thread(a)
#
#        # multithread
#        res = pool.map(run_thread, arguments)
#
#
#        for idx, result in enumerate(res):
#
#            result_masked_topk, sample_MRR, sample_P, sample_perplexity, msg, sample_em, sample_f1, sample_is_error, sample_pred_too_large, sample_pred_too_small, sample_should_be_empty, sample_should_be_not_empty, sample_anchor_outside, sample_mismatch= result
#
#            logger.info("\n" + msg + "\n")
#
#            sample = samples_b[idx]
#
#            element = {}
#            element["sample"] = sample
#            element["uuid"] = sample["uuid"]
#            element["token_ids"] = token_ids_list[idx]
#            element["masked_indices"] = masked_indices_list[idx]
#            element["label_index"] = label_index_list[idx]
#            element["masked_topk"] = result_masked_topk
#            element["sample_MRR"] = sample_MRR
#            element["sample_Precision"] = sample_P
#            element["sample_perplexity"] = sample_perplexity
#            element["sample_Precision1"] = result_masked_topk["P_AT_1"]
#            element['sample_em'] = sample_em
#            element['sample_f1'] = sample_f1
#
#            # print()
#            # print("idx: {}".format(idx))
#            # print("masked_entity: {}".format(result_masked_topk['masked_entity']))
#            # for yi in range(10):
#            #     print("\t{} {}".format(yi,result_masked_topk['topk'][yi]))
#            # print("masked_indices_list: {}".format(masked_indices_list[idx]))
#            # print("sample_MRR: {}".format(sample_MRR))
#            # print("sample_P: {}".format(sample_P))
#            # print("sample: {}".format(sample))
#            # print()
#
#            MRR += sample_MRR
#            Precision += sample_P
#            Precision1 += element["sample_Precision1"]
#
#            EM += sample_em
#            F1 += sample_f1
#            is_error += sample_is_error
#            pred_too_large += sample_pred_too_large
#            pred_too_small += sample_pred_too_small
#            should_be_empty += sample_should_be_empty
#            should_be_not_empty += sample_should_be_not_empty
#            anchor_outside += sample_anchor_outside
#            mismatch += sample_mismatch
#            # the judgment of the annouators recording whether they are
#            # evidence in the sentence that indicates a relation between two entities.
#            num_yes = 0
#            num_no = 0
#
#            if "judgments" in sample:
#                # only for Google-RE
#                for x in sample["judgments"]:
#                    if x["judgment"] == "yes":
#                        num_yes += 1
#                    else:
#                        num_no += 1
#                if num_no >= num_yes:
#                    samples_with_negative_judgement += 1
#                    element["judgement"] = "negative"
#                    MRR_negative += sample_MRR
#                    Precision_negative += sample_P
#                else:
#                    samples_with_positive_judgement += 1
#                    element["judgement"] = "positive"
#                    MRR_positive += sample_MRR
#                    Precision_positivie += sample_P
#
#            list_of_results.append(element)
#
#    #df = pd.DataFrame(example_results)
#    #df.to_csv('example_results.csv')
#
#    if viz:
#        with open('viz.pkl', 'wb') as wf:
#            pickle.dump(final_viz, wf)
#
#    pool.close()
#    pool.join()
#
#    # stats
#    # Mean reciprocal rank
#    MRR /= len(list_of_results)
#
#    # Precision
#    Precision /= len(list_of_results)
#    Precision1 /= len(list_of_results)
#
#    EM /= len(list_of_results)
#    F1 /= len(list_of_results)
#
#    msg = "all_samples: {}\n".format(len(all_samples))
#    msg += "list_of_results: {}\n".format(len(list_of_results))
#    msg += "global MRR: {}\n".format(MRR)
#    msg += "global Precision at 10: {}\n".format(Precision)
#    msg += "global Precision at 1: {}\n".format(Precision1)
#    msg += "global EM {}\n".format(EM)
#    msg += "global F1: {}\n".format(F1)
#
#    if samples_with_negative_judgement > 0 and samples_with_positive_judgement > 0:
#        # Google-RE specific
#        MRR_negative /= samples_with_negative_judgement
#        MRR_positive /= samples_with_positive_judgement
#        Precision_negative /= samples_with_negative_judgement
#        Precision_positivie /= samples_with_positive_judgement
#        msg += "samples_with_negative_judgement: {}\n".format(
#            samples_with_negative_judgement
#        )
#        msg += "samples_with_positive_judgement: {}\n".format(
#            samples_with_positive_judgement
#        )
#        msg += "MRR_negative: {}\n".format(MRR_negative)
#        msg += "MRR_positive: {}\n".format(MRR_positive)
#        msg += "Precision_negative: {}\n".format(Precision_negative)
#        msg += "Precision_positivie: {}\n".format(Precision_positivie)
#
#    logger.info("\n" + msg + "\n")
#    print("\n" + msg + "\n")
#
#    # dump pickle with the result of the experiment
#    all_results = dict(
#        list_of_results=list_of_results, global_MRR=MRR, global_P_at_10=Precision
#    )
#    with open("{}/result.pkl".format(log_directory), "wb") as f:
#        pickle.dump(all_results, f)
#
#    #logdf = pd.DataFrame(cf_results, columns=['Context', 'Subject', 'Template', 'Relation', 'Object', 'Score'])
#    #logdf.to_csv("{}/cf_results4.csv".format(log_directory))
#
#    #logdf = pd.DataFrame(qualitative_results, columns=['Template', 'Subject', 'Context', 'Object', 'Prediction', 'Expanded Prediction'])
#    #logdf.to_csv("{}/qual_results.csv".format(log_directory))
#
#    return Precision1, Precision, MRR, EM, F1, is_error, pred_too_large, pred_too_small, should_be_empty, should_be_not_empty, anchor_outside, mismatch


if __name__ == "__main__":
    print("Main") 
    #TODO: is this ever called from this file?
    #parser = options.get_eval_KB_completion_parser()
    #args = options.parse_args(parser)
    #main(args)
