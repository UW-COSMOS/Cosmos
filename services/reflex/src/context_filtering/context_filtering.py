import numpy as np
import fasttext
import sqlite3
import random
import spacy
import json
from collections import Counter

class Context_Filtering:
    def __init__(self, we_model_pth, db_dump, word_weight_pth, N=100):
        print("Loading fastext word embeddings ... ")
        self.word_emb = fasttext.load_model(we_model_pth)
        print("Connecting to db ... ")
        conn = sqlite3.connect(db_dump)

        self.db_cursor = conn.cursor()
        self.contexts_count = self.get_contexts_count()
        self.word_weight = Counter(json.load(open(word_weight_pth)))

        self.filter_tokens = ['.', ',', '(', ')', '</s>', '_._', ':', '-', ',', '_..._', '_:_']

        print("Loading spacy model ... ")
        self.nlp = spacy.load('en_core_web_lg')
        # Obtain a sample of N contexts
        self.random_contexts = self.get_random_contexts(N)
        #print(len(random_contexts))
        # Obtain sentence embedding for all the random contexts
        self.sent_emb_rc = []
        for rc in self.random_contexts:
            self.sent_emb_rc.append(self.compute_sentence_embedding(rc))
        self.sent_emb_rc = np.asarray(self.sent_emb_rc)
        # print(self.sent_emb_rc.shape)

    def get_contexts_count(self):
        '''
        Computes the total number of contexts in the db
        :return contexts_count - total number of contexts in the db
        '''
        self.db_cursor.execute("SELECT count(*) FROM documents")
        rows = self.db_cursor.fetchone()
        contexts_count = rows[0]
        return contexts_count

    def get_random_contexts(self, N):
        '''
        Sample contexts randomly from the db
        :param N - number of random contexts to sample
        :return rc - random contexts
        '''
        sample_idxs = random.sample(range(0, self.contexts_count), N)
        
        sql="select * from documents where rowid in ({seq})".format(seq=','.join(['?']*len(sample_idxs)))
        self.db_cursor.execute(sql, sample_idxs)
        rows = self.db_cursor.fetchall()
        rc = [row[1] for row in rows]
        return rc

    def compute_sentence_embedding(self, s):
        '''
        Compute the sentence embedding as the weighted avergage of word embeddings.
        :param s - sentence for which to compute embeddings
        :return sen_emb - sentence embedding
        '''
        doc = self.nlp(s.strip())
        we = [] # a vector of word embeddings
        weights = [] # a vector for word weights
        for token in doc:
            token_text = token.text
            #if token_text in self.filter_tokens:
            #    continue
            we.append(self.word_emb[token_text])
            weights.append(self.word_weight[token_text])
        we = np.asarray(we)
        weights = np.asarray(weights)
        return weights.dot(we)/ np.count_nonzero(weights)

    
    def compute_distributions(self, sent_emb_t, sent_emb_rc):
        '''
        Fits conditional multivariate gusssian distributions for sent_emb_t and sent_emb_rc with a tied covariance.
        :param sent_emb_t
        :param sent_emb_rc
        :return (mean_pos, mean_neg, shared_cov)
        '''
        mean_pos = np.expand_dims(sent_emb_t.mean(0), axis=0)
        mean_neg = np.expand_dims(sent_emb_rc.mean(0), axis=0)

        sent_emb_t = sent_emb_t - mean_pos
        sent_emb_rc = sent_emb_rc - mean_neg
        
        shared_cov = (np.matmul(sent_emb_t.T, sent_emb_t) + np.matmul(sent_emb_rc.T, sent_emb_rc))/(sent_emb_t.shape[0] + sent_emb_rc.shape[0])
        return mean_pos, mean_neg, shared_cov


    def get_mahalanobis_score(self, sent_emb_c, mean_pos, mean_neg, shared_cov):
        '''
        Computes mahalonobis distance from a point x from the distribution d
        :param sent_emb_c - Sentence embedding of the context
        :param mean_pos - Positive class distribution's mean.
        :param mean_neg - Negative class distribution's mean.
        :param shared_cov - Tied covariance of positive and negative class distribution.
        :return (True/Fale, score) - score is computed using mahalanobis distance
        '''

        shared_cov_inv = np.linalg.inv(shared_cov)

        # Calculate distance from positive class
        sent_emb_c = sent_emb_c - mean_pos
        pos_d = np.abs(np.matmul(np.matmul(sent_emb_c, shared_cov_inv), sent_emb_c.T).diagonal()[0])

        # Calculate distance from negative class
        sent_emb_c = sent_emb_c - mean_neg
        neg_d = np.abs(np.matmul(np.matmul(sent_emb_c, shared_cov_inv), sent_emb_c.T).diagonal()[0])
        
        #print(pos_d, neg_d)
        if pos_d < neg_d:
            return True, -pos_d
        else:
            return False, -neg_d

    def get_cosine_sim_score(self, emb1, emb2):
        return np.dot(emb1, emb2)/(np.linalg.norm(emb1) * np.linalg.norm(emb2))

    def get_cosine_set_score(self, template, context):
        '''
        :param template
        :param context
        :return score
        '''
        template = self.nlp(template.strip())
        context = self.nlp(context.strip())
        set_score = []
        for t_token in template:
            t_text = t_token.text
            max_score = 0
            for c_token in context:
                c_text = c_token.text
                score = self.get_cosine_sim_score(self.word_emb[c_text], self.word_emb[t_text])
                if score > max_score:
                    max_score = score
            #print(t_text, self.word_weight[t_text])
            set_score.append(self.word_weight[t_text] * max_score)
        #print(set_score)
        return np.average(set_score)

    def accept_context_mahal(self, context, subject, template, label):
        '''
        :param context
        :param subject
        :param template
        :param label
        :param N - number of random contexts in the sample
        :return True/False - wether to accept or reject the context
        '''

        # Process input
        template = template.replace('[X]', subject).replace('[Y]','')
        label = subject + " " + label 
        
        # Obtain sentence embedding for template
        sent_emb_t = []
        sent_emb_t.append(self.compute_sentence_embedding(template))
        sent_emb_t.append(self.compute_sentence_embedding(label))
        sent_emb_t = np.asarray(sent_emb_t)
        # print(sent_emb_t.shape)

        # Obtain sentence embeddings for current context
        sent_emb_c = self.compute_sentence_embedding(context)
        sent_emb_c = np.expand_dims(sent_emb_c, axis=0)
        # print(sent_emb_c.shape)
        # Compute distributions 
        mean_pos, mean_neg, shared_cov = self.compute_distributions(sent_emb_t, self.sent_emb_rc)

        return self.get_mahalanobis_score(sent_emb_c, mean_pos, mean_neg, shared_cov)
        
    def accept_context_cosine_sif(self, context, subject, template, label):
        '''
        :param context
        :param subject
        :param template
        :param label
        :param N - number of random contexts in the sample
        :return True/False - wether to accept or reject the context
        '''

        # Process input
        template = template.replace('[X]', subject).replace('[Y]','')+" " + label
        #context = context.replace(subject, '')
        # Obtain sentence embedding for template
        sent_emb_t = self.compute_sentence_embedding(template)
        # print(sent_emb_t.shape)

        # Obtain sentence embeddings for current context
        sent_emb_c = self.compute_sentence_embedding(context)
        # print(sent_emb_c.shape)

        return self.get_cosine_sim_score(sent_emb_t, sent_emb_c)

    def accept_context(self, context, subject, template, label):
        '''
        :param context
        :param subject
        :param template
        :param label
        :param N - number of random contexts in the sample
        :return True/False - wether to accept or reject the context
        '''

        # Process input
        template = template.replace('[X]', subject).replace('[Y]','') + label
        #print(template)
        #context = context.replace(subject, '')

        return self.get_cosine_set_score(template, context)
    

if __name__ == '__main__':
    we_model_pth = 'data/cc.en.300.bin' 
    db_dump = 'data/contexts.db'
    word_weight_pth = 'data/wiki_word_weight.json'
    cf = Context_Filtering(we_model_pth, db_dump, word_weight_pth)
    print('Testing ... ')

    result = cf.accept_context("Travis Hamonic (born August 16, 1990) is a Canadian professional ice hockey defenseman currently playing for the New York Islanders of the National Hockey League (NHL).", "Travis Hamonic", "[X] plays for [Y]", "drafted by")
    print(result)
    result = cf.accept_context("Austin Watson was born January 13, 1992, in Ann Arbor, Michigan, where he grew up with his father and mother, Mike and Mary", "Austin Watson","[X] plays for [Y]", "drafted by")
    print(result)