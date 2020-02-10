# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#
import spacy
from typing import List, Tuple, Dict
import numpy as np
import torch
import torch.nn.functional as F
ROBERTA_MASK = "<mask>"
ROBERTA_START_SENTENCE = "<s>"
ROBERTA_END_SENTENCE = "</s>"
ROBERTA_VOCAB_SIZE = 50266


class InferenceLM():
    def __init__(self):
        super().__init__()
        self.model = torch.hub.load('pytorch/fairseq', 'roberta.large', force_reload=True)
        self.bpe = self.model.bpe
        self.task = self.model.task
        self._build_vocab()
        self._init_inverse_vocab()
        self.max_sentence_length = 256
        self.cosine_similarity = torch.nn.CosineSimilarity(dim=0)
        self.filter_tokens = ['.', ',', '(', ')', '</s>', '_._', ':', '-', ',', '_..._', '_:_']
        self.nlp = spacy.load('en_core_web_lg')
        self._model_device = 'cpu'

    def _cuda(self) -> None:
        self.model.cuda()

    def _init_inverse_vocab(self):
        self.inverse_vocab = {w: i for i, w in enumerate(self.vocab)}

    def try_cuda(self) -> None:
        """Move model to GPU if one is available."""
        if torch.cuda.is_available():
            if self._model_device != 'cuda':
                print('Moving model to CUDA')
                self._cuda()
                self._model_device = 'cuda'
        else:
            print('No CUDA found')


    def _build_vocab(self) -> None:
        """

        Build processed vocab list from roberta vocabulary

        :return: vocab list

        """ 

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

    def create_cluster_centers_no_vecs(self, tokens: List[str], filter: bool = True) -> List[Tuple[str, str, int, int]]:
        """

        Compute cluster centers from relevant, expanded context tokens

        :param tokens: tokens to determine cluster centers for
        :param filter: determinese if tokens should be filtered
        :return: cluster center list of (token, word, start_ind, end_ind) tuples

        """ 

        cluster_centers = []
        stored_word = None
        for ind, token in enumerate(tokens):
            token = token.long()
            word = self.vocab[token]

            # Expland subwords to full expression 
            if word.startswith('_'): 
                word = word[1:-1]
                if filter:
                    if word in self.filter_tokens:
                        continue
                if stored_word is None:
                    stored_word = (token, word, ind+1, ind+1)
                else:
                    t = stored_word[0]
                    if self.vocab[stored_word[0]] in self.filter_tokens:
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

    def compute_mask_similarity_using_logprobs(self, context: str, template: str, subj: str, masked_idx: int, logprobs: List[float], logindices: List[int], k: int, cluster_centers: List[Tuple[str, str, int, int]], layer_num: int =11, all: bool = False)-> Tuple[Dict[int, float], List[Tuple[str, float]], str, str]: 
        """

        Compute the contextual similarity scores to redistribute probablility mass among tokens in context

        :param context: context tokens
        :param subj: subject to form sample root
        :masked_idx: mask index
        :logprobs: top k probabilities for tokens in q = [c,t] being anchor token
        :logindices: indices corresponding ot logprobs
        :k: evaluate over top k results to improve complexity
        :cluster_centers: cluster centers computed from context tokens
        :layer_num: which layer to evaluate
        :all: evaluate over all model layers
        :return: copy_logits: dict from vocab to redistributed probabilities centering anchor tokens in context
        :return: viz_seq: list of (token, score) tuples over each cluster center
        :return: tok: max valued token
        :return: expanded_tok: expanded token with NER phraser
        """ 

        with torch.no_grad():

            # Parse template tokens
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

                # Parse context, form q=[c,t]
                context = context.rstrip()
                context_spans = self.bpe.encode(context)
                text_spans_bpe = f'{ROBERTA_START_SENTENCE} {context_spans} {text_spans_bpe}'
                encoded = self.task.source_dictionary.encode_line(
                    text_spans_bpe, append_eos=True
                )
                len_encoded = len(encoded)

                encoded = encoded.unsqueeze(0).long().cuda()

                # Obtain vector embeddings for template tokens
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

                # For each cluster center, compute probability of this being anchor token
                if all:
                    for first_token, full_token, first_ind, last_ind in cluster_centers:
                        sum_feature = None
                        denom = 0

                        # Sum over all context tokens
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
                    start_ind = cluster_centers[simind][2]

                    if start_ind in ind_scores:
                        ind_scores[start_ind] += prob * simprob
                    else:
                        ind_scores[start_ind] =  prob * simprob

            # Update visual to account for multiple word results, find max scored token
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
            
            # Create dict from vocab to max valued item
            keys, values = zip(*list(ind_scores.items()))
            copy_logits = torch.zeros(ROBERTA_VOCAB_SIZE-1)
            for start_ind, value in ind_scores.items():
                dict_key = encoded.squeeze()[start_ind]
                if copy_logits[dict_key] == 0:
                    copy_logits[dict_key] = value
                else: # Only take the max value in the sequence (if the sequence contains repeats)
                    if value > copy_logits[dict_key].cpu().item():
                        copy_logits[dict_key] = value

            expanded_tok = self.map_to_chunks(tok, context)
            return copy_logits, viz_seq, tok, expanded_tok

    def map_to_chunks(self, token: str, context: str) -> str:
        """

        Expand anchor token to max length phrase containing token using NER chunker 

        :param token: anchor token to expand
        :param context: context to obtain phrase from 
        :return: longest chunk of context (entity or noun chunk) containing token
        
        """
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



    def voronoi_infer(self, context: str, template: str, subj: str, obj: str, k: int) -> Tuple[Dict[int, float], List[Tuple[str, float]], str, str] :
        """
        Obtain roberta model log probs representing probability of each token
        in context being the mask token

        :param context: context to search for token in 
        :param template: relation template
        :param subj
        :param obj
        :param k: number of top results to return 
        :return copy_probs: 
        :return viz_seq:  
         """
        self.try_cuda()
        with torch.no_grad():
            # Parse cloze form query q=[c,t]
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

            # Evaluate roberta model on q to obtain probabilities for each token being anchor token
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
            
            # Take top k anchor token predictions
            value_max_probs, index_max_probs = torch.topk(input=mask_probs,k=k,dim=0)
            
            # Computer cluster centers from context tokens
            cluster_centers = self.create_cluster_centers_no_vecs(context_encoded)
            copy_logits, viz_seq, anchor_tok, expanded_tok = self.compute_mask_similarity_using_logprobs(context, template, subj, masked_index, value_max_probs, index_max_probs, k, cluster_centers, all=True)
        return copy_logits.cpu(), viz_seq, anchor_tok, expanded_tok


