# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import spacy
import numpy as np
import torch
import torch.nn.functional as F
ROBERTA_MASK = "<mask>"
ROBERTA_START_SENTENCE = "<s>"
ROBERTA_END_SENTENCE = "</s>"
ROBERTA_VOCAB_SIZE = 50266


class Roberta():
    def __init__(self):
        super().__init__()
        self.model = torch.hub.load('pytorch/fairseq', 'roberta.large')
        self.bpe = self.model.bpe
        self.task = self.model.task
        self._build_vocab()
        self._init_inverse_vocab()
        self.max_sentence_length = 256 #args.max_sentence_length
        self.cosine_similarity = torch.nn.CosineSimilarity(dim=0)
        self.filter_tokens = ['.', ',', '(', ')', '</s>', '_._', ':', '-', ',', '_..._', '_:_']
        self.nlp = spacy.load('en_core_web_lg')
        self._model_device = 'cpu'

    def _cuda(self):
        self.model.cuda()

    def _init_inverse_vocab(self):
        self.inverse_vocab = {w: i for i, w in enumerate(self.vocab)}

    def try_cuda(self):
        """Move model to GPU if one is available."""
        if torch.cuda.is_available():
            if self._model_device != 'cuda':
                print('Moving model to CUDA')
                self._cuda()
                self._model_device = 'cuda'
        else:
            print('No CUDA found')


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

    def create_cluster_centers_no_vecs(self, tokens, filter=True):
        cluster_centers = []
        stored_word = None
        stops = ['.', ',', '(', ')', '</s>', '_._', ':', '-', ',', '_..._', '_:_']
        for ind, token in enumerate(tokens):
            token = token.long()
            word = self.vocab[token]
            if word.startswith('_'): #subword
                word = word[1:-1]
                if filter:
                    if word in self.filter_tokens:
                        continue
                if stored_word is None:
                    stored_word = (token, word, ind+1, ind+1)
                else:
                    t = stored_word[0]
                    if self.vocab[stored_word[0]] in stops:
                        t = token
                    stored_word = (t, stored_word[1] + word, stored_word[2], ind+1)
            else:
                if stored_word is None:
                    stored_word = (token, word, ind+1, ind+1)
                else:
                    cluster_centers.append(stored_word)
                    stored_word = None
                    if filter:
                        if word in self.filter_tokens:
                            continue
                    stored_word = (token, word, ind+1, ind+1)

        if stored_word is not None:
            cluster_centers.append(stored_word)
        return cluster_centers

    def compute_mask_similarity_using_logprobs(self, context, template, subj, masked_idx, logprobs, logindices, k, cluster_centers, layer_num=11, all=False):
        with torch.no_grad():
            probs = F.softmax(logprobs)
            ind_scores = {}
            if subj is not None:
                sample_root = template.replace('[X]', subj)
            else:
                sample_root = template
            for i in range(k):
                idx = logindices[i].item()

                prob = probs[i].item()
                word_form = self.vocab[idx]
                if word_form in self.filter_tokens:
                    continue
                if subj is not None:
                    sentence = sample_root.replace('[Y]', word_form)
                else:
                    sentence = sample_root.replace('[MASK]', word_form)
                encoded_word_form = self.task.source_dictionary.encode_line(self.bpe.encode(f' {word_form}'))
                text_spans_bpe = self.bpe.encode(sentence.rstrip())
                context = context.rstrip()
                context_spans = self.bpe.encode(context)
                text_spans_bpe = f'{ROBERTA_START_SENTENCE} {context_spans} {text_spans_bpe}'
                encoded = self.task.source_dictionary.encode_line(
                    text_spans_bpe, append_eos=True
                )
                len_encoded = len(encoded)

                encoded = encoded.unsqueeze(0).long().cuda()
    
                self.model.eval()
                self.model.model.eval()
                if all:
                    features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
                else:
                    features = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][layer_num].squeeze()

                if all:
                    mask_features = features[:, masked_idx, :].reshape(-1).unsqueeze(0)
                else:
                    mask_features = features[masked_idx, :].squeeze()

                similarities = []
                viz_seq = []
                if all:
                    for first_token, full_token, first_ind, last_ind in cluster_centers:
                        sum_feature = None
                        denom = 0
                        for i in range(last_ind - first_ind + 1):
                            if sum_feature is None:
                                sum_feature = features[:, first_ind+i, :].reshape(-1)
                            else:
                                sum_feature += features[:, first_ind+i, :].reshape(-1)
                            denom += 1
                        feature = sum_feature / denom
                        feature = feature.unsqueeze(1)
                        similarity = F.cosine_similarity(mask_features, torch.transpose(feature, 0, 1), dim=1)
                        similarities.append(similarity.squeeze())
                        viz_seq.append((full_token, similarity.squeeze().detach().cpu().item()))
                else:
                    for first_token, full_token, first_ind, last_ind in cluster_centers:
                        sum_feature = None
                        denom = 0
                        for i in range(last_ind - first_ind + 1):
                            if sum_feature is None:
                                sum_feature = features[first_ind+i, :].reshape(-1)
                            else:
                                sum_feature += features[first_ind+i, :].reshape(-1)
                            denom += 1
                        feature = sum_feature / denom
                        feature = feature.unsqueeze(1)
                        similarity = torch.mm(mask_features, feature)
                        similarities.append(similarity.squeeze())
                similarities = torch.stack(similarities)
 
                simprobs, siminds = torch.topk(similarities, k=len(similarities), dim=0)
                simprobs = F.softmax(simprobs)


                for simprob, simind in zip(simprobs, siminds):
                    ai = cluster_centers[simind][2]
                    #]if self.vocab[ai] == 'He':
                    if ai in ind_scores:
                        ind_scores[ai] += prob * simprob
                    else:
                        ind_scores[ai] =  prob * simprob

            viz_seq = []
            max_score = 0
            tok = ''
            for first_token, full_token, first_ind, last_ind in cluster_centers:
                score = ind_scores[first_ind] if first_ind in ind_scores else torch.tensor(0)
                score = score.detach().cpu().numpy().item()
                if score > max_score:
                    tok = full_token
                    max_score = score
                viz_seq.append((full_token, score))

            keys, values = zip(*list(ind_scores.items()))
            copy_logits = torch.zeros(ROBERTA_VOCAB_SIZE-1)
            for start_ind, value in ind_scores.items():
                dict_key = encoded.squeeze()[start_ind]
                if copy_logits[dict_key] == 0:
                    copy_logits[dict_key] = value
                else: # Only take the max value in the sequence (if the sequence contains repeats)
                    if value > copy_logits[dict_key].cpu().item():
                        copy_logits[dict_key] = value

            tok2 = self.map_to_chunks(tok, context)
            return copy_logits, viz_seq, tok, tok2

    def map_to_chunks(self, token, context):
        doc = self.nlp(context)
        matched_chunks = []
        for chunk in doc.ents:
            if token in chunk.text:
                matched_chunks.append(chunk)
        if len(matched_chunks) == 0:
            for chunk in doc.noun_chunks:
                if token in chunk.text:
                    matched_chunks.append(chunk)
        if len(matched_chunks) == 0:
            return token
        return str(max(matched_chunks, key=lambda x: len(x)))



    def voronoi_infer(self, context, template, subj, obj, k, vecs=None):
        self.try_cuda()
        with torch.no_grad():
            if subj is not None:
                sample_root = template.replace('[X]', subj)
                sample = sample_root.replace('[Y]', obj)
            else:
                sample = template.replace('[MASK]', obj)
            text_spans = sample.split(ROBERTA_MASK)
            context = context.rstrip()
            context_spans = self.bpe.encode(context)
            text_spans_bpe_temp = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
            text_spans_bpe = f'{ROBERTA_START_SENTENCE} {context_spans} {text_spans_bpe_temp}'
            context = context.rstrip()
            context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=False
            )
            len_context_encoded = len(context_encoded)
            encoded = self.task.source_dictionary.encode_line(
                text_spans_bpe, append_eos=True
            )
            len_encoded = len(encoded)
            while len_encoded > 500:
                # For now, we prune the context
                context = context[:-10]
                context_spans = self.bpe.encode(context)
                context_encoded = self.task.source_dictionary.encode_line(
                    context_spans, append_eos=False
                )
                len_context_encoded = len(context_encoded)
                text_spans_bpe = f'{ROBERTA_START_SENTENCE} {context_spans} {text_spans_bpe_temp}'
                encoded = self.task.source_dictionary.encode_line(
                    text_spans_bpe, append_eos=True
                )
                len_encoded = len(encoded)
            encoded = encoded.unsqueeze(0).long().cuda()
            encoded_token_list = encoded.long().cpu().numpy()
            masked_indices_list = []
            masked_index = (encoded.squeeze() == self.task.mask_idx).nonzero().squeeze()
            with torch.no_grad():
                self.model.eval()
                self.model.model.eval()
                self.model.model.cuda()
                log_probs, extra = self.model.model(
                    encoded,
                    features_only=False,
                    return_all_hiddens=False,
                )
            log_probs = log_probs.squeeze()

            mask_probs = log_probs[masked_index]
            value_max_probs, index_max_probs = torch.topk(input=mask_probs,k=k,dim=0)

            cluster_centers = self.create_cluster_centers_no_vecs(context_encoded)
            copy_probs, viz_seq, chunk1, chunk2 = self.compute_mask_similarity_using_logprobs(context, template, subj, masked_index, value_max_probs, index_max_probs, k, cluster_centers, all=True)
        return copy_probs.cpu(), viz_seq, chunk1, chunk2


