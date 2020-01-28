# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import spacy
import numpy as np
import evaluation_metrics as metrics
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
        self.max_sentence_length = 256 #args.max_sentence_length
        self.cosine_similarity = torch.nn.CosineSimilarity(dim=0)
        self.filter_tokens = ['.', ',', '(', ')', '</s>', '_._', ':', '-', ',', '_..._', '_:_']
        self.nlp = spacy.load('en_core_web_lg')

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

    def decode(self, tokens: torch.LongTensor):
        assert tokens.dim() == 1
        tokens = tokens.detach().cpu().numpy()
        if tokens[0] == self.task.source_dictionary.bos():
            tokens = tokens[1:]  # remove <s>
        eos_mask = (tokens == self.task.source_dictionary.eos())
        doc_mask = eos_mask[1:] & eos_mask[:-1]
        sentences = np.split(tokens, doc_mask.nonzero()[0] + 1)
        sentences = [self.bpe.decode(self.task.source_dictionary.string(s)) for s in sentences]
        if len(sentences) == 1:
            return sentences[0]
        return sentences

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

    def get_word_embedding(self, token):
        with torch.no_grad():
            embedding = self.model.model.decoder.sentence_encoder.embed_tokens(token)
            return embedding

    def get_ft_word_embedding(self, token, vecs):
        word = self.vocab[token]
        if word.startswith('_'):
            word = word[1:-1]
        return torch.tensor(vecs[word])


    def create_cluster_centers(self, tokens, vecs):
        cluster_centers = []
        stored_word = None
        for token in tokens:
            token = token.long()
            word = self.vocab[token]
            if word.startswith('_'): #subword
                word = word[1:-1]
                if word in self.filter_tokens:
                    continue
                if stored_word is None:
                    stored_word = (token, word)
                else:
                    stored_word = (stored_word[0], stored_word[1] + word)
            else:
                if stored_word is None:
                    stored_word = (token, word)
                else:
                    cluster_centers.append((stored_word[0], torch.tensor(vecs[stored_word[1]]), stored_word[1]))
                    stored_word = None
                    if word in self.filter_tokens:
                        continue
                    stored_word = (token, word)

        cluster_centers.append((stored_word[0], torch.tensor(vecs[stored_word[1]]), stored_word[1]))
        return cluster_centers

    def debug_cluster_centers(self, cluster_centers, vecs):
        for val, vec, word in cluster_centers:
            token = val.item()
            ms = vecs.most_similar(word)
            print('MOST SIMILAR\n_______________')
            print(self.vocab[token])
            print(ms)
            print(word)
            print('-------')

    def get_sentence_embedding_cls(self, sentence):
        return self.model.model.extract_features(sentence)[0][:, 0, :].reshape(-1)

    def find_contextual_match(self, context, template, subj, candidate_set):
        with torch.no_grad():
            # with utils.eval(self.model.model):
            self.model.eval()
            self.model.model.eval()
            sample = template.replace('[X]', subj)
            context_scores = []
            for candidate in candidate_set:
                sample = sample.replace('[Y]', candidate)
                query_spans = self.bpe.encode(template)
                query_spans = f'{ROBERTA_START_SENTENCE} {query_spans}'
                query_encoded = self.task.source_dictionary.encode_line(
                        query_spans, append_eos=True
                    ).long().unsqueeze(0)

                query_features = self.get_sentence_embedding_cls(query_encoded)

                context_spans = self.bpe.encode(context.rstrip())
                end = None
                score = 0
                for i in range(len(context_spans)):
                    cspan = context_spans[:len(context_spans)-i]
                    cspan = f'{ROBERTA_START_SENTENCE} {cspan}'
                    c_encoded = self.task.source_dictionary.encode_line(
                            cspan, append_eos=True
                        ).long().unsqueeze(0)
                    context_features = self.get_sentence_embedding_cls(c_encoded)
                    similarity = self.cosine_similarity(query_features, context_features)
                    if similarity > score:
                        score = similarity
                        end = len(context_spans)-i

                best_context = context_spans[:end]
                start = None
                score = 0
                for i in range(len(best_context)):
                    cspan = best_context[i:]
                    cspan = f'{ROBERTA_START_SENTENCE} {cspan}'
                    c_encoded = self.task.source_dictionary.encode_line(
                            cspan, append_eos=True
                        ).long().unsqueeze(0)
                    context_features = self.get_sentence_embedding_cls(c_encoded)
                    similarity = self.cosine_similarity(query_features, context_features)
                    if similarity > score:
                        score = similarity
                        start = i
                final_context = best_context[start:]
                context_scores.append((final_context, similarity))
            return context_scores

    def compute_clusters(self, context, vecs):
        cluster_centers = []
        clusters = {}
        context = context.squeeze()
        cluster_centers = self.create_cluster_centers(context, vecs)
        #self.debug_cluster_centers(cluster_centers, vecs)

        # -1, mask
        for key in range(ROBERTA_VOCAB_SIZE-1):
            vembedding = self.get_ft_word_embedding(torch.tensor(key).long(), vecs)
            dists = [self.cosine_similarity(cc[1], vembedding) for cc in cluster_centers]

            stacked_dists = torch.stack(dists)
            index = torch.argmax(stacked_dists)

            clusters[key] = cluster_centers[index][0]
            #if self.vocab[key] == 'Guitar':
            #    cluster_centers

        return clusters, cluster_centers

    def renormalize_log_probs_to_clusters(self, clusters, logprobs):
        # logprobs bsz x vocab size
        logprobs = logprobs.squeeze()
        logprobs = F.softmax(logprobs)
        final_scores = {}
        logs = []
        for ind, val in enumerate(logprobs):
            cc = clusters[ind]
            if self.vocab[ind] == '</s>':
                continue
            if val < 0:
                continue
            if val > 0.05:
                logs.append((cc, ind))
            if cc in final_scores:
                final_scores[cc] = final_scores[cc] + val
            else:
                final_scores[cc] = val

        inds = []
        values = []
        for k, v in final_scores.items():
            inds.append(k)
            values.append(v)
        vstack = torch.stack(values)
        #vstack = F.softmax(vstack)
        probs, indices = torch.topk(vstack, k=10 if len(vstack) >= 10 else len(vstack), dim=0)
        final_indices = []
        for index in indices:
            i = index.item()
            ind = inds[i]
            final_indices.append(ind)
        final_indices = torch.tensor(final_indices).long()
        return probs, final_indices, logs


    def compute_mask_similarity(self, sentence, masked_idx, context_len):
        for key in range(ROBERTA_VOCAB_SIZE-1):
            vembedding = self.get_ft_word_embedding(torch.tensor(key).long(), vecs)
            dists = [self.cosine_similarity(cc[1], vembedding) for cc in cluster_centers]

            stacked_dists = torch.stack(dists)
            index = torch.argmax(stacked_dists)

            clusters[key] = cluster_centers[index][0]
            #if self.vocab[key] == 'Guitar':
            #    cluster_centers
        for ind, val in enumerate(logprobs):
            cc = clusters[ind]
            if self.vocab[ind] == '</s>':
                continue
            if val < 0:
                continue
            if val > 0.05:
                logs.append((cc, ind))
            if cc in final_scores:
                final_scores[cc] = final_scores[cc] + val
            else:
                final_scores[cc] = val
        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            features = self.model.model.extract_features(sentence, return_all_hiddens=True)[1]['inner_states'][11].squeeze()

        mask_features = features[masked_idx, :].squeeze()
        similarities = []
        for feature in features[:context_len, :]:
            similarity = self.cosine_similarity(mask_features, feature)
            similarities.append(similarity)
        similarities = torch.stack(similarities)
 
        probs, indices = torch.topk(similarities, k=10 if len(similarities) >= 10 else len(similarities), dim=0)
        # the first one is going to be mask with itself
        probs = probs[1:]
        indices = indices[1:]

        actual_indices = []
        for ind in indices:
            actual_indices.append(sentence.squeeze()[ind])
        actual_indices = torch.stack(actual_indices)
        return probs, actual_indices

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


    def compute_template_similarity_same_space(self, context, template, subj, obj):
        self.try_cuda()
        sample_root = template.replace('[X]', subj)
        sample = sample_root.replace('[Y]', obj)
        text_spans = sample.split(ROBERTA_MASK)
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        context_spans = self.bpe.encode(context.rstrip())
        text_spans_bpe = f'{ROBERTA_START_SENTENCE} {text_spans_bpe} {context_spans}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=True
        ).long().cuda().unsqueeze(0)

        context_spans = f'{context_spans}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)
        len_context = len(context_encoded)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()
            features_template = torch.mean(features[1+len_context:-1, :], 0)
            features_context = torch.mean(features[1:len_context+2, :], 0)

        sim = self.cosine_similarity(features_template, features_context)
        return sim


    def compute_template_similarity_greedy_subset(self, context, template, subj, obj):
        self.try_cuda()
        sample_root = template.replace('[X]', subj)
        sample = sample_root.replace('[Y]', obj)
        text_spans = sample.split(ROBERTA_MASK)
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        text_spans_bpe = f'{ROBERTA_START_SENTENCE} {text_spans_bpe}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=True
        ).long().cuda().unsqueeze(0)

        context_spans = self.bpe.encode(context.rstrip())
        context_spans = f'{ROBERTA_START_SENTENCE} {context_spans}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features_template = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()
            features_context = self.model.model.extract_features(context_encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()

        
        masked_index = (encoded.squeeze() == self.task.mask_idx).nonzero().squeeze()
        #features_context = torch.cat((features_context[:masked_index], features_context[masked_index+1:]))

        features_template_mean = torch.mean(features_template, 0)
        max_score = 0
        end = None
        for i in range(len(context_encoded)):
            features_context_mean = torch.mean(features_context[:-(i+1), :], 0)
            sim = self.cosine_similarity(features_template_mean, features_context_mean)
            if sim > max_score:
                max_score = sim
                end = i
        start = 0
        for i in range(end):
            features_context_mean = torch.mean(features_context[i:end+1, :], 0)
            sim = self.cosine_similarity(features_template_mean, features_context_mean)
            if sim > max_score:
                max_score = sim
                start = i
        max_span = context_encoded[start:end+1]
        for c in max_span.squeeze():
            result = []
            result.append(self.vocab[c.item()])

        return max_score, start, end

    def compute_template_similarity(self, context, template, subj, obj):
        self.try_cuda()
        sample_root = template.replace('[X]', subj)
        sample = sample_root.replace('[Y]', obj)
        text_spans = sample.split(ROBERTA_MASK)
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        text_spans_bpe = f'{ROBERTA_START_SENTENCE} {text_spans_bpe}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=True
        ).long().cuda().unsqueeze(0)

        context_spans = self.bpe.encode(context.rstrip())
        context_spans = f'{ROBERTA_START_SENTENCE} {context_spans}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features_template = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()
            features_context = self.model.model.extract_features(context_encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()



        features_template_mean = torch.mean(features_template, 0)
        features_context_mean = torch.mean(features_context, 0)
        sim = self.cosine_similarity(features_template_mean, features_context_mean)
        return sim



    def compute_template_precision(self, context, template, subj, obj):
        self.try_cuda()
        sample_root = template.replace('[X]', subj)
        sample = sample_root.replace('[Y]', obj)
        text_spans = sample.split(ROBERTA_MASK)
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        text_spans_bpe = f'{ROBERTA_START_SENTENCE} {text_spans_bpe}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=True
        ).long().cuda().unsqueeze(0)

        context_spans = self.bpe.encode(context.rstrip())
        context_spans = f'{ROBERTA_START_SENTENCE} {context_spans}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features_template = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()
            features_context = self.model.model.extract_features(context_encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()

        masked_index = (encoded.squeeze() == self.task.mask_idx).nonzero().squeeze()
        s = None
        denom = 0
        for ind, i in enumerate(features_template):
            if ind == masked_index:
                continue
            i = i.unsqueeze(0)
            result = torch.mm(i, torch.transpose(features_context, 0, 1)).squeeze()
            val, ind = torch.max(result, 0)
            if s is None:
                s = val
            else:
                s += val
            denom += 1
        precision = s / denom
        return precision


    def compute_template_precision2(self, context, template, subj, obj):
        self.try_cuda()
        sample_root = template.replace('[X]', subj)
        sample = sample_root.replace('[Y]', obj)
        text_spans = sample.split(ROBERTA_MASK)
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        text_spans_bpe = f'{ROBERTA_START_SENTENCE} {text_spans_bpe}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=True
        ).long().cuda().unsqueeze(0)

        context_spans = self.bpe.encode(context.rstrip())
        context_spans = f'{ROBERTA_START_SENTENCE} {context_spans}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features_template = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()
            features_context = self.model.model.extract_features(context_encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()

        masked_index = (encoded.squeeze() == self.task.mask_idx).nonzero().squeeze()
        s = None
        denom = 0
        for ind, i in enumerate(features_template):
            if ind == masked_index or ind == 0 or ind == len(features_template)-1:
                continue
            i = i.unsqueeze(0)
            shape = list(features_context.shape)
            i = i.repeat(shape[0], 1)
            #features_context = torch.transpose(features_context, 0, 1)
            #result = torch.mm(i, torch.transpose(features_context, 0, 1)).squeeze()
            result = self.cosine_similarity(i, features_context)
            val, ind = torch.max(result, 0)

            if s is None:
                s = val
            else:
                s += val
            denom += 1
        precision = s / denom
        return precision

    def compute_template_sequence_precision(self, context, template, subj, obj):
        self.try_cuda()
        sample_root = template.replace('[X]', subj)
        sample = sample_root.replace('[Y]', obj)
        text_spans = sample.split(ROBERTA_MASK)
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        text_spans_bpe = f'{ROBERTA_START_SENTENCE} {text_spans_bpe}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=True
        ).long().cuda().unsqueeze(0)

        context_spans = self.bpe.encode(context.rstrip())
        context_spans = f'{ROBERTA_START_SENTENCE} {context_spans}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features_template = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()
            features_context = self.model.model.extract_features(context_encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()

        masked_index = (encoded.squeeze() == self.task.mask_idx).nonzero().squeeze()
        s = None
        denom = 0
        inds = []
        for ind, i in enumerate(features_template):
            if ind == masked_index or ind == 0 or ind == len(features_template)-1:
                continue
            i = i.unsqueeze(0)
            shape = list(features_context.shape)
            i = i.repeat(shape[0], 1)
            #features_context = torch.transpose(features_context, 0, 1)
            #result = torch.mm(i, torch.transpose(features_context, 0, 1)).squeeze()
            result = F.cosine_similarity(i, features_context)
            val, ind = torch.max(result, 0)
            inds.append(ind)

        inds = torch.stack(inds)

        
        min_ind, _ = torch.min(inds, 0)
        max_ind, _ = torch.max(inds, 0)
        # Now we measure how similar this sequence is compared to the original
        new_context = features_context[min_ind:max_ind+1, :]
        new_context_features = torch.mean(new_context, 0)
        template_features = torch.mean(features_template, 0)
        sim = self.cosine_similarity(new_context_features, template_features)
        max_span = context_encoded.squeeze()[min_ind.item():max_ind.item()+1]
        for c in max_span.squeeze():
            result = []
            result.append(self.vocab[c.item()].strip())

        return sim



    def compute_template_precision_weighted(self, context, template, subj, obj):
        self.try_cuda()
        subject_weight = 0.5
        relation_weight = 0.5
        object_weight = 0
        if obj in subj:
            return 0
        sample_root = template.replace('[Y]', obj)
        sample = sample_root.replace('[X]', subj)
        text_spans = sample.split(ROBERTA_MASK)
        # Encode the subject
        if obj != ROBERTA_MASK:
            obj = f' {obj}'
            object_bpe = self.bpe.encode(obj)
            object_encoded = self.task.source_dictionary.encode_line(object_bpe, append_eos=False).long().cuda()
            obj_encoded_len = len(object_encoded)
        else:
            object_encoded = self.task.source_dictionary.encode_line(obj, append_eos=False).long().cuda()
            obj_encoded_len = len(object_encoded)
        subj = f' {subj}'
        subject_bpe = self.bpe.encode(subj)
        subject_encoded = self.task.source_dictionary.encode_line(subject_bpe, append_eos=False).long().cuda()
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        text_spans_bpe = f'{ROBERTA_START_SENTENCE} {text_spans_bpe}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=True
        ).long().cuda()#.unsqueeze(0)
        subj_tok_weight = subject_weight / len(subject_encoded)
        obj_tok_weight = object_weight / obj_encoded_len
        
        rel_tok_weight = relation_weight / (len(encoded) - len(subject_encoded) - 2 - obj_encoded_len)
        for i in range(len(encoded)):
            if torch.allclose(encoded[i:i+len(subject_encoded)], subject_encoded):
                subject_indices = range(i, i+len(subject_encoded))
                break

        for i in range(len(encoded)):
            if torch.allclose(encoded[i:i+len(object_encoded)], object_encoded):
                object_indices = range(i, i+len(object_encoded))
                break

        encoded = encoded.unsqueeze(0)


        context_spans = self.bpe.encode(context.rstrip())
        context_spans = f'{ROBERTA_START_SENTENCE} {context_spans}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features_template = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()
            features_context = self.model.model.extract_features(context_encoded, return_all_hiddens=True)[1]['inner_states'][11].squeeze()

        masked_index = (encoded.squeeze() == self.task.mask_idx).nonzero().squeeze()
        s = None
        denom = 0
        for ind, i in enumerate(features_template):
            if ind == 0 or ind == len(features_template)-1:
                continue
            i = i.unsqueeze(0)
            #shape = list(features_context.shape)
            #i = i.repeat(shape[0], 1)
            #features_context = torch.transpose(features_context, 0, 1)
            #result = torch.mm(i, torch.transpose(features_context, 0, 1)).squeeze()
            result = torch.mm(i, torch.transpose(features_context, 0, 1)).squeeze()
            val, tok_ind= torch.max(result, 0)

            if s is None:
                if ind in subject_indices:
                    s = subj_tok_weight * val
                elif ind in object_indices:
                    s = obj_tok_weight * val
                else:
                    s = rel_tok_weight * val
            else:
                if ind in subject_indices:
                    s += subj_tok_weight * val
                elif ind in object_indices:
                    s += obj_tok_weight * val
                else:
                    s += rel_tok_weight * val
            #denom += 1
        precision = s #/ denom
        return precision

    def integrated_gradients(self, context, template, subj, obj, k, vecs=None):
        self.try_cuda()

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
        #print(masked_index)
        #clusters, cluster_centers = self.compute_clusters(context_encoded, vecs)
        #candidate_set = [c[2] for c in cluster_centers]
        #scores = self.find_contextual_match(context, template, subj, candidate_set)

        self.model.eval()
        self.model.model.eval()
        self.model.model.cuda()
        cluster_centers = self.create_cluster_centers_no_vecs(context_encoded, filter=False)
        feature_weights = self.model.model(encoded, integrated_gradients=True, masked_index=masked_index).detach().cpu()
        copy_logits = torch.zeros(ROBERTA_VOCAB_SIZE-1)
        for ind, value in enumerate(feature_weights):
            hit = False
            tok = None
            for first_token, full_token, first_ind, last_ind in cluster_centers:
                if ind >= first_ind and ind <= last_ind:
                    tok = first_token
                    hit = True
                    break

            if not hit:
                continue
            dict_key = tok #encoded.squeeze()[ind]
            if copy_logits[dict_key] == 0:
                copy_logits[dict_key] = value
            else: 
                copy_logits[dict_key] += value
        value_max_probs, index_max_probs = torch.topk(input=copy_logits,k=10,dim=0)
        index_max_probs = index_max_probs.numpy().astype(int)
        value_max_probs = value_max_probs.detach().numpy()

        result_masked_topk, return_msg = metrics._print_top_k(value_max_probs, index_max_probs, self.vocab, 10, None)

        return copy_logits.detach().cpu()

    def compute_set_precision(self, context, template, subj, obj):
        self.try_cuda()
        sample_root = template.replace('[X]', subj)
        sample = sample_root.replace('[Y]', obj)
        text_spans = sample.split(ROBERTA_MASK)
        text_spans_bpe = f' {ROBERTA_MASK} '.join([self.bpe.encode(ts.rstrip()) for ts in text_spans])
        text_spans_bpe = f'{text_spans_bpe}'
        encoded = self.task.source_dictionary.encode_line(
            text_spans_bpe, append_eos=False
        )
        len_encoded = len(encoded)

        sample_root2 = template.replace('[X]', '')
        sample2 = sample_root.replace('[Y]', '')
        template_bpe = self.bpe.encode(sample2.rstrip())
        template_span = f'{template_bpe}'
        encoded_template = self.task.source_dictionary.encode_line(
            template_span, append_eos=False
        )
        template_cluster_centers = self.create_cluster_centers_no_vecs(encoded_template)

        context_spans = self.bpe.encode(context.rstrip())
        context_encoded_for_clusters = self.task.source_dictionary.encode_line(
                context_spans, append_eos=False
            )
        context_cluster_centers = self.create_cluster_centers_no_vecs(context_encoded_for_clusters)
        context_spans = f'{ROBERTA_START_SENTENCE} {context_spans} {text_spans_bpe}'
        context_encoded = self.task.source_dictionary.encode_line(
                context_spans, append_eos=True
            ).long().cuda().unsqueeze(0)

        with torch.no_grad():
            self.model.eval()
            self.model.model.eval()
            #features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
            features = torch.stack(self.model.model.extract_features(context_encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()


        final_sim_score = 0
        matching_terms = []
        min_index = 0
        for template_first_token, template_full_token, template_first_ind, template_last_ind in template_cluster_centers:
            sum_feature = None
            denom = 0
            for i in range(template_last_ind - template_first_ind + 1):
                if sum_feature is None:
                    sum_feature = features[:, template_first_ind+i, :].reshape(-1)
                else:
                    sum_feature += features[:, template_first_ind+i, :].reshape(-1)
                denom += 1
            feature = sum_feature / denom
            max_sim = 0
            matching_token = None
            matched_index = None
            
            for context_first_token, context_full_token, context_first_ind, context_last_ind in context_cluster_centers:
                sum_feature = None
                denom = 0
                if context_first_ind <= min_index:
                    continue
                for i in range(context_last_ind - context_first_ind + 1):
                    if sum_feature is None:
                        sum_feature = features[:, context_first_ind+i, :].reshape(-1)
                    else:
                        sum_feature += features[:, context_first_ind+i, :].reshape(-1)
                    denom += 1
                context_feature = sum_feature / denom

                similarity = torch.dot(context_feature, feature)#F.cosine_similarity(context_feature, feature, dim=0)
                similarity = similarity.squeeze().detach().cpu().item()
                if similarity > max_sim:
                    matched_index = context_last_ind
                    max_sim = similarity 
                    matching_token = context_full_token
            final_sim_score += max_sim
            matching_terms.append((template_full_token, matching_token, max_sim))
            min_index = matched_index
        return final_sim_score, matching_terms



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
                        #similarity = torch.mm(mask_features, feature)
                        similarity = F.cosine_similarity(mask_features, torch.transpose(feature, 0, 1), dim=1)
                        similarities.append(similarity.squeeze())
                        viz_seq.append((full_token, similarity.squeeze().detach().cpu().item()))
                else:
                    for first_token, full_token, first_ind, last_ind in cluster_centers:
                        #tok = self.vocab[encoded.squeeze()[i].item()]
                        #if tok in self.filter_tokens or tok[1:-1] in self.filter_tokens:
                        #    similarities.append(torch.tensor(0).float().cuda())
                        #    continue
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




    def infer_answer_seq_simple(self, context, template, subj, masked_idx, logprobs, logindices, k, cluster_centers, layer_num=11, all=False):
        with torch.no_grad():
            probs = F.softmax(logprobs)
            ind_scores = {}
            lind_scores = {}
            find_scores = {}
            if subj is not None:
                sample_root = template.replace('[X]', subj)
            else:
                sample_root = template
            #cc_precisions = []
            #for cc in cluster_centers:
            #    cc_precision = self.compute_template_precision_weighted(context, template, subj, cc[1])
            #    cc_precisions.append(cc_precision)
            #    print(f'precision: {cc_precision}')
            for i in range(k):
                idx = logindices[i].item()

                prob = probs[i].item()
                word_form = self.vocab[idx]
                if word_form in self.filter_tokens:
                    continue
                #if word_form == '</s>':
                #    continue
                if subj is not None:
                    sentence = sample_root.replace('[Y]', word_form)
                else:
                    sentence = sample_root.replace('[MASK]', word_form)
                #print(word_form)
                # TODO: Check back here! SMOOTH OVER ENTIRE PROPOSED WORD????
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
                #masked_idx = (encoded.squeeze() == encoded_word_form).nonzero().squeeze()

    
                self.model.eval()
                self.model.model.eval()
                features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()

                mask_features = features[:, masked_idx, :].reshape(-1).unsqueeze(0)

                similarities = []
                viz_seq = []
                for first_token, full_token, first_ind, last_ind in cluster_centers:
                    #if self.vocab[encoded.squeeze()[first_ind].item()] in self.filter_tokens:
                    #    similarities.append(torch.tensor(0).float().cuda())
                    #    continue
                    sum_feature = None
                    denom = 0
                    # It's possible we have chopped off a cluster center
                    for i in range(last_ind - first_ind + 1):
                        if sum_feature is None:
                            sum_feature = features[:, first_ind+i, :].reshape(-1)
                        else:
                            sum_feature += features[:, first_ind+i, :].reshape(-1)
                        denom += 1
                    feature = sum_feature / denom
                    feature = feature.unsqueeze(1)
                    #similarity = torch.mm(mask_features, feature)
                    similarity = F.cosine_similarity(mask_features, torch.transpose(feature, 0, 1), dim=1)
                    similarities.append(similarity.squeeze())
                    viz_seq.append((full_token, similarity.squeeze().detach().cpu().item()))
                similarities = torch.stack(similarities)
 
                #simprobs, siminds = torch.topk(similarities, k=len(similarities), dim=0)
                simprobs = F.softmax(similarities)


                for ind, simprob in enumerate(simprobs):
                    _, _, find, lind = cluster_centers[ind]
                    enc_seq = encoded.squeeze()[find:lind+1]
                    decoded_str = self.decode(enc_seq)
                    if decoded_str in ind_scores:
                        ind_scores[decoded_str] += prob * simprob
                    else:
                        ind_scores[decoded_str] =  prob * simprob

                    if find in find_scores:
                        find_scores[find] += prob * simprob
                    else:
                        find_scores[find] =  prob * simprob

                    if lind in lind_scores:
                        lind_scores[lind] += prob * simprob
                    else:
                        lind_scores[lind] =  prob * simprob

            first_results = find_scores.items()
            first_results = sorted(first_results, key=lambda x: x[1], reverse=True)
            last_results = lind_scores.items()
            last_results = sorted(last_results, key=lambda x: x[1], reverse=True)
            final_results = []
            for f in first_results:
                for l in last_results:
                    if l[0] + 1 <= f[0]:
                        continue
                    final_results.append((f[0], l[0], f[1] * l[1]))
            print('-------')
            for i in range(0, 5):

                enc_seq = encoded.squeeze()[final_results[i][0]:final_results[i][1]+1]
                decoded_str = self.decode(enc_seq)
                print(f'Pred rank {i}: Prediciton: {decoded_str}, Score: {final_results[i][2]}')
            
            results = ind_scores.items()

            results = sorted(results, key=lambda x: x[1], reverse=True)
            print('-------')
            for i in range(0, 5):
                print(f'Pred rank {i}: Prediciton: {results[i][0]}, Score: {results[i][1]}')
            
            return results

            viz_seq = []
            for first_token, full_token, first_ind, last_ind in cluster_centers:
                score = ind_scores[first_ind] if first_ind in ind_scores else torch.tensor(0)
                score = score.detach().cpu().numpy().item()
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

            return copy_logits, viz_seq

    def infer_answer_seq(self, context, template, subj, masked_idx, logprobs, logindices, k, cluster_centers, layer_num=11, all=False):
        with torch.no_grad():
            probs = F.softmax(logprobs)
            ind_scores = {}
            if subj is not None:
                sample_root = template.replace('[X]', subj)
            else:
                sample_root = template
            #cc_precisions = []
            #for cc in cluster_centers:
            #    cc_precision = self.compute_template_precision_weighted(context, template, subj, cc[1])
            #    cc_precisions.append(cc_precision)
            #    print(f'precision: {cc_precision}')
            print(context)
            print(template)
            for i in range(k):
                idx = logindices[i].item()

                prob = probs[i].item()
                word_form = self.vocab[idx]
                if word_form in self.filter_tokens:
                    continue
                #if word_form == '</s>':
                #    continue
                if subj is not None:
                    sentence = sample_root.replace('[Y]', f'{word_form} {ROBERTA_MASK}')
                else:
                    sentence = sample_root.replace('[MASK]', word_form)
                #print(word_form)
                # TODO: Check back here! SMOOTH OVER ENTIRE PROPOSED WORD????
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
                #masked_idx = (encoded.squeeze() == encoded_word_form).nonzero().squeeze()
                #print(masked_idx)

    
                self.model.eval()
                self.model.model.eval()
                if all:
                    features = torch.stack(self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states']).squeeze()
                else:
                    features = self.model.model.extract_features(encoded, return_all_hiddens=True)[1]['inner_states'][layer_num].squeeze()

                if all:
                    mask_features = features[:, masked_idx, :].reshape(-1).unsqueeze(0)
                    #print(features[:, masked_idx, :])
                else:
                    mask_features = features[masked_idx, :].squeeze()

                results = []
                for ind, cc in enumerate(cluster_centers):
                   first_token, full_token, first_ind, last_ind = cc
                   sim = 0
                   j = 0
                   result = (first_ind, last_ind, 0)
                   while True:
                       sum_feature = None
                       denom = 0
                       # It's possible we have chopped off a cluster center
                       for i in range(last_ind - first_ind + 1):
                           if sum_feature is None:
                               sum_feature = features[:, first_ind+i, :].reshape(-1)
                           else:
                               sum_feature += features[:, first_ind+i, :].reshape(-1)
                           denom += 1
                       feature = sum_feature / denom
                       feature = feature.unsqueeze(1)
                       similarity = F.cosine_similarity(mask_features, torch.transpose(feature, 0, 1), dim=1).squeeze()
                       if similarity >= sim:
                           sim = similarity
                           j += 1
                           result = (first_ind, last_ind, sim)
                           if ind+j >= len(cluster_centers):
                               break
                           if j > 2:
                               break
                           #if ind-j <= 0:
                           #    break
                           #first_ind = cluster_centers[ind-j][3]
                           last_ind = cluster_centers[ind+j][3]
                       else:
                           j += 1
                           if ind+j >= len(cluster_centers):
                               break
                           if j > 2:
                               break
                           last_ind = cluster_centers[ind+j][3]

                   results.append(result)

                finds, linds, sims = zip(*results)
                sims = torch.stack(sims)
                sims = F.softmax(sims)

                for find, lind, sim in zip(finds, linds, sims):
                    enc_seq = encoded.squeeze()[find:lind+1]
                    decoded_str = self.decode(enc_seq)
                    if decoded_str in ind_scores:
                        ind_scores[decoded_str] += prob * sim
                    else:
                        ind_scores[decoded_str] = prob * sim


            results = ind_scores.items()

            results = sorted(results, key=lambda x: x[1], reverse=True)
            print('-------')
            for i in range(0, 5):
                print(f'Pred rank {i}: Prediciton: {results[i][0]}, Score: {results[i][1]}')
            
            return results


    def voronoi_sequence_infer(self, context, template, subj, obj, k, vecs=None):
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
                # with utils.eval(self.model.model):
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
            cluster_centers = self.expand_cluster_centers(cluster_centers, 3)
            answers = self.infer_answer_seq_simple(context, template, subj, masked_index, value_max_probs, index_max_probs, k, cluster_centers, all=True)

            return mask_probs, answers[0][0]

    def expand_cluster_centers(self, cluster_centers, N):
        new_cluster_centers = []
        for ind, cc in enumerate(cluster_centers):
            token, word_form, first_ind, last_ind = cc
            for n in range(N):
                if ind+n == len(cluster_centers):
                    break
                last_cc = cluster_centers[ind+n]
                new_last_ind = last_cc[3]
                new_cluster_centers.append((token, word_form, first_ind, new_last_ind))
        return new_cluster_centers


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
            #print(masked_index)
            #clusters, cluster_centers = self.compute_clusters(context_encoded, vecs)
            #candidate_set = [c[2] for c in cluster_centers]
            #scores = self.find_contextual_match(context, template, subj, candidate_set)

            with torch.no_grad():
                # with utils.eval(self.model.model):
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
            #print(mask_probs)
            value_max_probs, index_max_probs = torch.topk(input=mask_probs,k=k,dim=0)
            #result_masked_topk, return_msg = print_top_k(value_max_probs, index_max_probs, self.vocab, k, None, max_printouts=10)
            #print(return_msg)
            #probs, indices, logs = self.renormalize_log_probs_to_clusters(clusters, mask_probs)
            #print("ALL LAYERS FLATTENED")
            #print("=========================")
            cluster_centers = self.create_cluster_centers_no_vecs(context_encoded)
            copy_probs, viz_seq, chunk1, chunk2 = self.compute_mask_similarity_using_logprobs(context, template, subj, masked_index, value_max_probs, index_max_probs, k, cluster_centers, all=True)
            #value_max_probs, index_max_probs = torch.topk(input=copy_probs,k=10,dim=0)
            #index_max_probs = index_max_probs.numpy().astype(int)
            #value_max_probs = value_max_probs.detach().numpy()

            #result_masked_topk, return_msg = metrics._print_top_k(value_max_probs, index_max_probs, self.vocab, 10, None)
            #print(return_msg)
            #msim_probs, msim_indices = self.compute_mask_similarity(encoded, masked_index, len_context_encoded)
            #for l in logs:
            #    x, y = l
            #    x = x.item()
            #    y = y
            #    print(f'{self.vocab[x]} -------- {self.vocab[y]}')
            #result_masked_topk, return_msg = print_top_k(probs, indices, self.vocab, k if len(indices) >= k else len(indices), None, max_printouts=10)
            #result_masked_topk, return_msg = print_top_k(msim_probs, msim_indices, self.vocab, k if len(msim_indices) >= k else len(msim_indices), None, word_forms=msim_indices, max_printouts=10)
            #element = {'i' : i, 'token_idx': idx, 'log_prob': log_prob, 'token_word_form': word_form}
            #print(return_msg)
            #print("=========================")

            #for i in range(12):
            #    print(f"LAYERS {i+1}")
            #    print("=========================")
            #    msim_probs, msim_indices = self.compute_mask_similarity_using_logprobs(context, sample_root,  masked_index, len_context_encoded, value_max_probs, index_max_probs, k, cluster_centers, layer_num=i)
            #    #msim_probs, msim_indices = self.compute_mask_similarity(encoded, masked_index, len_context_encoded)
            #    #for l in logs:
            #    #    x, y = l
            #    #    x = x.item()
            #    #    y = y
            #    #    print(f'{self.vocab[x]} -------- {self.vocab[y]}')
            #    #result_masked_topk, return_msg = print_top_k(probs, indices, self.vocab, k if len(indices) >= k else len(indices), None, max_printouts=10)
            #    result_masked_topk, return_msg = print_top_k(msim_probs, msim_indices, self.vocab, k if len(msim_indices) >= k else len(msim_indices), None, max_printouts=10)
            #    #element = {'i' : i, 'token_idx': idx, 'log_prob': log_prob, 'token_word_form': word_form}
            #    print(return_msg)
            #    print("=========================")
            #precision = torch.tensor(0)

            return copy_probs.cpu(), viz_seq, chunk1, chunk2

    def infer(self, context, template, subj, obj, k, vecs=None):
        self.try_cuda()
        with torch.no_grad():
            #precision = self.compute_template_precision(context, template, subj, obj)
            #precision = self.compute_template_precision_weighted(context, template, subj, obj)
            #precision = self.compute_template_sequence_precision(context, template, subj, obj)
            #precision = self.compute_template_similarity(context, template, subj, obj)
            #precision, start, end = self.compute_template_similarity_greedy_subset(context, template, subj, obj)
            #precision = self.compute_template_similarity_same_space(context, template, subj, obj)
            #print('PRECISION')
            #print(precision)
            #print('________')
            if subj is not None:
                sample_root = template.replace('[X]', subj)
                sample = sample_root.replace('[Y]', obj)
            else:
                sample = template.replace('[MASK]', obj)
            #print('***')
            #print(f'{context} {template}')
            #print('***')
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
            #print(masked_index)
            #clusters, cluster_centers = self.compute_clusters(context_encoded, vecs)
            #candidate_set = [c[2] for c in cluster_centers]
            #scores = self.find_contextual_match(context, template, subj, candidate_set)

            with torch.no_grad():
                # with utils.eval(self.model.model):
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

            return mask_probs.cpu()

    def get_batch_generation(self, sentences_list, logger=None, try_cuda=True):
        if not sentences_list:
            return None
        if try_cuda:
            self.try_cuda()
        #for sentence in sentence_list
            # while the prediction starts with an underscore, keep decoding

        tensor_list = []
        masked_indices_list = []
        max_len = 0
        output_tokens_list = []
        for masked_inputs_list in sentences_list:

            tokens_list = []

            for idx, masked_input in enumerate(masked_inputs_list):

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
            log_probs, extra = self.model.model(
                batch_tokens.long().to(device=self._model_device),
                features_only=False,
                return_all_hiddens=False,
            )

        # Need to decode log probabilities to actual word form
        # masked_indices_list has list of masked_indicies
        # log_probs is the log_probs for EVERY position. You need to decode at the MASKED position
        #masked_indices = masked_indices[:1]

        #masked_index = masked_indices[0]
        #log_probs = log_probs[masked_index]
        # mask_topk is a torch.topk over the MASK log probs (your k for topk is just 1, so you can just use torch.max)
        #for i in range(mask_topk):
        #    filtered_idx = index_max_probs[i].item() # This is the index of the top probability vocab word


        #    word_form = self.vocab[idx]
        #    if word_form.startswith('_'):
        #        prediction = prediction + word_form[1:-1]
        #        sentence = sentence.replace(MASK, prediction)
        #        sentence = f'{sentence} {MASK}'


        return log_probs.cpu(), output_tokens_list, masked_indices_list#, prediction

    def get_contextual_embeddings(self, sentences_list, try_cuda=True):
        # TBA
        return None
