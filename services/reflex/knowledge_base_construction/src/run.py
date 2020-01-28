import retrieve_docs
from retrieve_docs import retrieve_docs_for_concept
import glob
from shutil import copyfile
import os
import argparse
import click
from types import SimpleNamespace
from multiprocessing_bpe_encoder import main as run_mbpe
import subprocess
from roberta_connector import Roberta
import lama.modules.base_connector as base
import torch
import torch.nn.functional as F
from spacy.lang.en import English
import spacy
from collections import defaultdict
import gensim.models.fasttext as fasttext
import gensim.models.keyedvectors as kv
from lama.modules.base_connector import *

#nlp = English()
#nlp.add_pipe(nlp.create_pipe('sentencizer'))

db_dump = '/data/wikipedia/docs.db'
tfidf_model = '/data/wikipedia/docs-tfidf-ngram=2-hash=16777216-tokenizer=simple.npz'
k = 2
c = 'Walter Parazaider'
train_write = os.path.join('/data/', 'train_doc.txt')
encoder_path = '/data/encoder.json'
vocab_path = '/data/vocab.bpe'
output_write = os.path.join('/data/', 'output_train.bpe')
dict_path = os.path.join('/data/', 'dict.txt')
destdir = os.path.join('/data/', 'bin')
#template = '[X] plays the [Y]'
template = '[X] plays [Y]'
num_workers = 60
#MODEL_NAME = 'checkpoint_1_600.pt'
MODEL_NAME = 'model.pt'
MAX_DECODE_LEN = 1
BEAM_SIZE = 100
MAX_HYPOTHESIS_LEN = 1
TRAIN_SCRIPT = '/kbc/distill.sh'
ENCODER_LAYERS = 12
ENCODER_EMBED_DIM = 768
ENCODER_FFN_EMBED_DIM = 3072
ENCODER_ATTENTION_HEADS = 12
FASTTEXT_MODEL_PATH = '/data/cc.en.300.bin'
MIN_LEN = 10
#vecs = kv.FastTextKeyedVectors.load('/data/ft_vecs.vec')
#vecs = fasttext.load_facebook_vectors(FASTTEXT_MODEL_PATH)
#vecs.save('/data/ft_vecs.vec')

def decode_voronoi_infer(model, doc, template, subject, k):
    #d = nlp(doc)
    #sentences = [sent.string.strip() for sent in d.sents]
    template = f' {template}'
    sentences = doc.split('\n')
    sentences = [s.strip() for s in sentences]
    sentences = [s for s in sentences if len(s) > 0]
    res = []
    for sentence in sentences:
        enc = model.task.source_dictionary.encode_line(model.bpe.encode(sentence), append_eos=False)
        if len(enc) < MIN_LEN:
            continue
        result = model.voronoi_infer(sentence, template, subject, base.ROBERTA_MASK, k)
        res.append((result[0]['log_prob'], result[0], sentence))
    return res

def decode_infer(model, doc, template, subject, k):
    #d = nlp(doc)
    #sentences = [sent.string.strip() for sent in d.sents]
    template = f' {template}'
    sentences = doc.split('\n')
    sentences = [s.strip() for s in sentences]
    sentences = [s for s in sentences if len(s) > 0]
    res = []
    for sentence in sentences:
        enc = model.task.source_dictionary.encode_line(model.bpe.encode(sentence), append_eos=False)
        if len(enc) < MIN_LEN:
            continue
        result = model.infer(sentence, template, subject, base.ROBERTA_MASK, k)
        res.append((result['log_prob'], result['token_word_form'], sentence))
    return res

def run_infer():
    docs = retrieve_docs_for_concept(c, db_dump, tfidf_model, 1)
    PARAMETERS = {
        "bert_vocab_name": "vocab.txt",
        "max_sentence_length": 100,
        "lm": "roberta",
        "label": "roberta_base",
        "models_names": ["roberta"],
        "roberta_model_name": MODEL_NAME,
        "roberta_vocab_name": "dict.txt",
        "roberta_model_dir": "/data",
        "encoder_layers": ENCODER_LAYERS,
        "encoder_embed_dim": ENCODER_EMBED_DIM,
        "encoder_ffn_embed_dim": ENCODER_FFN_EMBED_DIM,
        "encoder_attention_heads": ENCODER_ATTENTION_HEADS,
    }
    model_args = argparse.Namespace(**PARAMETERS)

    model = Roberta(model_args)
    results = []
    for doc in docs:
        res = decode_voronoi_infer(model, doc, template, c, 10)
        results.extend(res)
    ranked = sorted(results, key=lambda x: x[0], reverse=True)
    for i in range(10):
        print(ranked[i])


def run(template, subject, ranker, conn, model, k=10):
    temp = template.replace('[X]', subject)
    temp = temp.replace('[Y]', '')
    docs = retrieve_docs.retrieve_docs_for_concept(temp, ranker, conn, 1)
    #PARAMETERS = {
    #    "bert_vocab_name": "vocab.txt",
    #    "max_sentence_length": 100,
    #    "lm": "roberta",
    #    "label": "roberta_base",
    #    "models_names": ["roberta"],
    #    "roberta_model_name": MODEL_NAME,
    #    "roberta_vocab_name": "dict.txt",
    #    "roberta_model_dir": "/data",
    #    "encoder_layers": ENCODER_LAYERS,
    #    "encoder_embed_dim": ENCODER_EMBED_DIM,
    #    "encoder_ffn_embed_dim": ENCODER_FFN_EMBED_DIM,
    #    "encoder_attention_heads": ENCODER_ATTENTION_HEADS,
    #}
    #model_args = argparse.Namespace(**PARAMETERS)

    #model = Roberta(model_args)
    results = []
    for doc in docs:
        #doc = sample_example
        #doc = sample_example1
        res = decode_infer(model, doc, template, subject, k)
        results.extend(res)
        #break
    precisions, topks, contexts = zip(*results)
    precisions = torch.stack(precisions)
    p_softs = F.softmax(precisions, dim=0)
    final_results = []
    for i in range(len(p_softs)):
        p_soft = p_softs[i]
        ks = topks[i]
        context = contexts[i]
        for t in ks:
            final_results.append((t['token_word_form'], p_soft, context, t['log_prob']))
            # For now, just get the top value for the context
            break
    words, probs, contexts, word_probs = zip(*final_results)
    probs = torch.stack(probs)
    ranked_vals, ranked_inds = torch.topk(probs, k=len(probs), dim=0)
    ranked_results = []
    for i, rind in enumerate(ranked_inds):
        ind = rind.item()
        word = words[ind]
        context = contexts[ind]
        word_prob = word_probs[ind]
        prob = ranked_vals[i]
        ranked_results.append((word, context, prob, word_prob))

    final_probs = torch.zeros(ROBERTA_VOCAB_SIZE)
    for word, context, prob, word_prob in ranked_results:
        word = word.strip()
        word = f' {word}'
        object_bpe = model.bpe.encode(word)
        object_encoded = model.task.source_dictionary.encode_line(object_bpe, append_eos=False).long().cuda()
        val = object_encoded[0]
        if final_probs[val] == 0:
            final_probs[val] = prob

    return final_probs





if __name__ == '__main__':
    run_infer()



