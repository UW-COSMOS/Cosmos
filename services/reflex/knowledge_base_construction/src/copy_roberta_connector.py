# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
from fairseq.models.roberta import RobertaModel
from fairseq import utils
import torch
import torch.nn.functional as F
from lama.modules.base_connector import *
import pickle
from collections import defaultdict
import fasttext
import logging
logger = logging.getLogger("fasttext")  # or __name__ for current module
logger.setLevel(logging.INFO)

def print_top_k(value_max_probs, index_max_probs, vocab, mask_topk, index_list, word_forms=None, max_printouts = 10):
    result = []
    msg = "\n| Top{} predictions\n".format(max_printouts)
    for i in range(mask_topk):
        if word_forms is not None:
            word_form = word_forms[i]
            idx=i
        else:
            filtered_idx = index_max_probs[i].item()
    
            if index_list is not None:
                # the softmax layer has been filtered using the vocab_subset
                # the original idx should be retrieved
                idx = index_list[filtered_idx]
            else:
                idx = filtered_idx
            word_form = vocab[idx]

        log_prob = value_max_probs[i].item()
        wf = word_form.lower()
        #if wf.startswith('sax'):
        #    print(i)
        #    print(wf)
        #    print(log_prob)

        if i < max_printouts:
            msg += "{:<8d}{:<20s}{:<12.3f}\n".format(
                i,
                word_form,
                log_prob
            )
        element = {'i' : i, 'token_idx': idx, 'log_prob': log_prob, 'token_word_form': word_form}
        result.append(element)
    return result, msg


class RobertaVocab(object):
    def __init__(self, roberta):
        self.roberta = roberta

    def __getitem__(self, arg):
        value = ""
        try:
            predicted_token_bpe = self.roberta.task.source_dictionary.string([arg])
            if (
                predicted_token_bpe.strip() == ROBERTA_MASK
                or predicted_token_bpe.strip() == ROBERTA_START_SENTENCE
            ):
                value = predicted_token_bpe.strip()
            else:
                value = self.roberta.bpe.decode(str(predicted_token_bpe)).strip()
        except Exception as e:
            print(e)
        return value


class Roberta(Base_Connector):
    def __init__(self, args):
        super().__init__()
        roberta_model_dir = args.roberta_model_dir
        roberta_model_name = args.roberta_model_name
        roberta_vocab_name = args.roberta_vocab_name
        self.dict_file = "{}/{}".format(roberta_model_dir, roberta_vocab_name)
        self.model = RobertaModel.from_pretrained(
            roberta_model_dir, checkpoint_file=roberta_model_name
        )
        self.bpe = self.model.bpe
        self.task = self.model.task
        self._build_vocab()
        self._init_inverse_vocab()
        self.max_sentence_length = args.max_sentence_length
        self.cosine_similarity = torch.nn.CosineSimilarity(dim=0)
        self.filter_tokens = ['.', ',', '(', ')', '</s>', '_._', ':', '-', ',', '_..._', '_:_']

    def _cuda(self):
        self.model.cuda()

    def _build_vocab(self):
        self.vocab = []
        for key in range(ROBERTA_VOCAB_SIZE):
            predicted_token_bpe = self.task.source_dictionary.string([key])
            try:
                value = self.bpe.decode(predicted_token_bpe)

                if value[0] == " ":  # if the token starts with a whitespace
                    value = value.strip()
                else:
                    # this is subword information
                    value = "_{}_".format(value)

                if value in self.vocab:
                    value = "{}_{}".format(value, key)

                self.vocab.append(value)

            except Exception as e:
                self.vocab.append(predicted_token_bpe.strip())

    def get_value(self, arg):
        value = ""
        try:
            predicted_token_bpe = self.source_dictionary.string([arg])
            if (
                predicted_token_bpe.strip() == ROBERTA_MASK
                or predicted_token_bpe.strip() == ROBERTA_START_SENTENCE
            ):
                value = predicted_token_bpe.strip()
            else:
                value = self.bpe.decode(str(predicted_token_bpe)).strip()
        except Exception as e:
            print("Exception {} for input {}".format(e, arg))
        return value

    def get_id(self, input_string):
        # Roberta predicts ' London' and not 'London'
        string = " " + str(input_string).strip()
        text_spans_bpe = self.bpe.encode(string.rstrip())
        tokens = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=False
        )
        return tokens.long()


    def get_batch_generation(self, sentences_list, logger=None, try_cuda=True):
        if not sentences_list:
            return None
        if try_cuda:
            self.try_cuda()

        tensor_list = []
        masked_indices_list = []
        max_len = 0
        output_tokens_list = []
        orig_spans = []
        for masked_inputs_list in sentences_list:

            tokens_list = []

            for idx, masked_input in enumerate(masked_inputs_list):
                orig_spans.append(masked_input)

                # 2. sobstitute [MASK] with <mask>
                masked_input = masked_input.replace(MASK, ROBERTA_MASK)

                text_spans = masked_input.split(ROBERTA_MASK)
                text_spans_bpe = (
                    (" {0} ".format(ROBERTA_MASK))
                    .join(
                        [
                            self.bpe.encode(text_span.rstrip())
                            for text_span in text_spans
                        ]
                    )
                    .strip()
                )

                prefix = ""
                if idx == 0:
                    prefix = ROBERTA_START_SENTENCE

                tokens_list.append(
                    self.task.source_dictionary.encode_line(
                        prefix + " " + text_spans_bpe, append_eos=True
                    )
                )

            tokens = torch.cat(tokens_list)[: self.max_sentence_length]
            output_tokens_list.append(tokens.long().cpu().numpy())

            if len(tokens) > max_len:
                max_len = len(tokens)
            tensor_list.append(tokens)
            masked_index = (tokens == self.task.mask_idx).nonzero().numpy()
            for x in masked_index:
                masked_indices_list.append([x[0]])

        pad_id = self.task.source_dictionary.pad()
        tokens_list = []
        for tokens in tensor_list:
            pad_lenght = max_len - len(tokens)
            if pad_lenght > 0:
                pad_tensor = torch.full([pad_lenght], pad_id, dtype=torch.int)
                tokens = torch.cat((tokens, pad_tensor))
            tokens_list.append(tokens)

        batch_tokens = torch.stack(tokens_list)

        with torch.no_grad():
            # with utils.eval(self.model.model):
            self.model.eval()
            self.model.model.eval()
            attns = []
            def hook_attn(m, i, o):
                output, attn = o
                attns.append(attn)
            #self.model.model.decoder.sentence_encoder.layers[0].self_attn.register_forward_pre_hook(hook)
            for layer in self.model.model.decoder.sentence_encoder.layers:
                layer.register_forward_hook(hook_attn)
            #self.model.model.decoder.sentence_encoder.register_forward_hook(hook_attn)
            log_probs, extra = self.model.model(
                batch_tokens.long().to(device=self._model_device),
                features_only=False,
                return_all_hiddens=False,
            )
            per_sentence_attns = []
            for ind, span in enumerate(orig_spans):
                sentence_attentions = []
                for attn in attns:
                    tok_tok_attn = attn[0][ind]
                    per_head_attn = attn[1][ind]
                    sentence_attentions.append((tok_tok_attn, per_head_attn))
                per_sentence_attns.append((span, sentence_attentions))
                    

        return log_probs.cpu(), output_tokens_list, masked_indices_list, per_sentence_attns

    def get_contextual_embeddings(self, sentences_list, try_cuda=True):
        # TBA
        return None
