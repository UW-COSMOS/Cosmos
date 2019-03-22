import os
import re
import string
import pandas as pd
from stanfordnlp.server import CoreNLPClient
from db_models import Equation, Variable, TableX
from sqlalchemy.dialects import postgresql
from fonduer.meta import Meta
from fonduer.parser.models import Sentence

from stanfordnlp.server import CoreNLPClient

db_connect_str = "postgres://postgres:password@localhost:5432/cosmos_unicode_1"
stop_words = [ "a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "could", "did", "do", "does", "doing", "down", "during", "each", "eq", "equation", "equations", "few", "for", "from", "further", "had", "has", "have", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "it", "it's", "its", "itself", "let's", "me", "more", "most", "my", "myself", "nor", "of", "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves", "out", "over", "own", "same", "she", "she'd", "she'll", "she's", "should", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "us", "very", "was", "we", "we'd", "we'll", "we're", "we've", "were", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "would", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves"]

def all_alpha(word):
    for char in word:
        if not char.isalpha():
            return False
    return True

def alpha_ratio(token):
    token = token.replace(' ','')
    if len(token) == 0:
        return 0
    num_alpha = 0
    for char in token:
        if char.isalpha():
            num_alpha += 1
    return float(num_alpha)/len(token)

def good_entity(token):
    if alpha_ratio(token) < 0.7:
        #print('Bad one:')
        #print(token)
        return False
    words = token.split()
    for word in words:
        if all_alpha(word) and word not in stop_words:
            return True
    return False

def get_token_index(char_positions, text):
    space = [' ','\n','\t']
    res = []
    index = 0

    start = False
    
    for i in range(len(text)-1):
        if text[i] not in space:
            start = True
        if text[i] in space and text[i+1] not in space and start:
            index += 1
        if i in char_positions:
            res.append(index)
    if len(text)-1 in char_positions:
        res.append(index)
    return res
    
def get_phrase_info(sentenses, text):
    index_base = 0
    for sent in sentenses:
        token_value = {}
        token_pos = {}
        token_begin_char = {}
        for token in sent.token:
            index = token.tokenBeginIndex
            token_value[index] = token.value
            token_pos[index] = token.pos
            token_begin_char[index] = token.beginChar
        for triple in sent.openieTriple:
            sub_pos = []
            sub_begin_char = []
            sub = triple.subject
            for subToken in triple.subjectTokens:
                sub_index = subToken.tokenIndex
                sub_pos.append(token_pos[index_base+sub_index])
                sub_begin_char.append(token_begin_char[index_base+sub_index])
            sub_indices = get_token_index(sub_begin_char, text)
            yield {'text':triple.subject,'indices':sub_indices,'pos':sub_pos}

            obj_pos = []
            obj_begin_char = []
            obj = triple.object
            for objToken in triple.objectTokens:
                obj_index = objToken.tokenIndex
                obj_pos.append(token_pos[index_base+obj_index])
                obj_begin_char.append(token_begin_char[index_base+obj_index])
            obj_indices = get_token_index(obj_begin_char, text)
            yield {'text':triple.object,'indices':obj_indices,'pos':obj_pos}
        index_base += len(token_value)

def isSinleVerb(pos_list):
    if len(pos_list) == 1 and pos_list[0].startswith('VB'):
        return True
    return False

def strip_stop_word(token):
    words = token.split()
    if len(words) == 0:
        return ''
    if words[0].lower() in stop_words:
        words[0] = ''
    if words[-1].lower() in stop_words:
        words[-1] = ''
    new_token = ' '.join(words).strip()
    if alpha_ratio(new_token) < 0.7:
        return ''
    else:
        return token

def remove_symbol(phrase, indices, sent_id, doc_id, vars_index):
    words = phrase.split()
    if len(words) == 0:
        return ''

    for i in range(len(words)):   
        if (doc_id, sent_id, indices[i]) in vars_index:
            words[i] = ''

    new_token = ' '.join(words).strip()
    if alpha_ratio(new_token) < 0.7:
        return ''
    else:
        return phrase

def build_table_X(db, corenlp):
    os.environ["CORENLP_HOME"] = corenlp

    with open('words_alpha.txt') as word_file:
        valid_words = set(word_file.read().split())
    session = Meta.init(db).Session()
    variables = session.query(Variable).order_by(Variable.equation_id)
    equations = session.query(Equation).order_by(Equation.id)
    sentences = session.query(Sentence).order_by(Sentence.id)
    with CoreNLPClient(annotators=['openie','pos']) as client:
        vars_used_index = []
        for eqt in equations:
            vars_in_eqt = variables.filter(Variable.equation_id == eqt.id)
            if vars_in_eqt.count() == 0:
                print('No Variable found for equation ' + str(eqt.id))
            else:
                vars_used = []
                sent_used = []
                entities = []
                phrases_top = []
                phrases_bottom = []
                phrases_left = []
                phrases_right = []
                phrases_page = []
                
                for var in vars_in_eqt:
                    vars_used_index.append((eqt.document_id, var.sentence_id, var.sentence_offset))
                    text = var.text.strip(',').strip('.').strip('?')
                    if text not in vars_used:
                        vars_used.append(text)
                for var in vars_in_eqt:
                    sent_id = var.sentence_id
                    target_sent = sentences.filter(Sentence.id == sent_id)[0]

                    top = target_sent.top
                    bottom = target_sent.bottom
                    left = target_sent.left
                    right = target_sent.right
                    page = target_sent.page
                    

                    if sent_id not in sent_used:
                        sent_used.append(sent_id)
                        sent_text = var.sentence_text
                        ann = client.annotate(sent_text)
                        phrase_info_all = []
                        indices_all = []
                        for phrase_info in get_phrase_info(ann.sentence, sent_text):
                            phrase_info_all.append(phrase_info)
                        phrase_info_all = sorted(phrase_info_all, key=lambda a: len(a['indices']))

                        for p_i in range(len(phrase_info_all)):
                            phrase_info_all[p_i]['isSubStr'] = False
                            for p_j in range(p_i+1,len(phrase_info_all)):
                                setA = set(phrase_info_all[p_i]['indices'])
                                setB = set(phrase_info_all[p_j]['indices'])
                                if setA.issubset(setB):
                                    phrase_info_all[p_i]['isSubStr'] = True

                        for phrase_info in phrase_info_all:
                            if not phrase_info['isSubStr'] and not isSinleVerb(phrase_info['pos']):
                                valid_phrase = phrase_info['text']
                                valid_pos = phrase_info['pos']
                                valid_indices = phrase_info['indices']

                                valid_phrase = remove_symbol(valid_phrase, valid_indices, sent_id, eqt.document_id, vars_used_index)

                                valid_phrase = re.sub('[' + string.punctuation + ']', '', valid_phrase)
                                valid_phrase = strip_stop_word(valid_phrase)
        

                                if good_entity(valid_phrase) and len(valid_phrase) > 0 and valid_phrase not in vars_used and valid_phrase not in entities:
                                    entities.append(valid_phrase)
                                    top_tmp = []
                                    bottom_tmp = []
                                    left_tmp = []
                                    right_tmp = []
                                    page_tmp = []
                                    top_str = ''
                                    bottom_str = ''
                                    left_str = ''
                                    right_str = ''
                                    page_str = ''
                                    for valid_index in valid_indices:
                                        if valid_index >= len(top):
                                            print('top:')
                                            print(top)
                                            print('index:')
                                            print(valid_indices)
                                            print('sentense_id:')
                                            print(sent_id)
                                        top_tmp.append(top[valid_index])
                                        bottom_tmp.append(bottom[valid_index])
                                        left_tmp.append(left[valid_index])
                                        right_tmp.append(right[valid_index])
                                        page_tmp.append(page[valid_index])

                                    df = pd.DataFrame({'top':top_tmp,'bottom':bottom_tmp, 'left':left_tmp, 'right':right_tmp, 'page':page_tmp})
                                    maxV = df.groupby('top').max()
                                    minV = df.groupby('top').min()

                                    for index, row in minV.iterrows():
                                        top_str += str(index)
                                        top_str += ' ' 
                                        left_str += str(row['left'])
                                        left_str += ' '
                                        bottom_str += str(row['bottom'])
                                        bottom_str += ' '
                                        page_str += str(row['page'])
                                        page_str += ' '
                                    for index, row in maxV.iterrows():
                                        right_str += str(row['right'])
                                        right_str += ' '
                                    phrases_top.append(top_str)
                                    phrases_left.append(left_str)
                                    phrases_right.append(right_str)
                                    phrases_bottom.append(bottom_str)
                                    phrases_page.append(page_str)
                                    
                
                x = TableX(
                    equation_id=eqt.id,
                    symbols=vars_used,
                    phrases=entities,
                    phrases_top = phrases_top,
                    phrases_bottom = phrases_bottom,
                    phrases_left = phrases_left,
                    phrases_right = phrases_right,
                    phrases_page = phrases_page,
                )

                session.add(x)

        session.commit()


if __name__ == '__main__':
    build_table_X(db_connect_str)
