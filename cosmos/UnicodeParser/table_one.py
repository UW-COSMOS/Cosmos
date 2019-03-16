from fonduer.meta import Meta
from sqlalchemy import Column, Integer, String, Text
from sqlalchemy.dialects import postgresql
from fonduer.parser.models import Document, Sentence
from db_models import Equation, Variable
from link import Latex
import re
import string

db_connect_str = "postgres://postgres:vangle@localhost:5432/cosmos5"
stop_words = ['a', 'all', 'am', 'an', 'and', 'any', 'are', 'as', 'at', 'be', 'but', 'by', 'can', 'did', 'do', 'few', \
'for', 'get', 'had', 'has', 'he', 'her', 'him', 'his', 'how', 'if', 'in', 'is', 'it', 'its', 'me', \
'my', 'nor', 'of', 'on', 'or', 'our', 'out', 'own', 'set', 'she', 'so', 'the', 'to', 'too', 'use', 'up', \
'was', 'we', 'who', 'why', 'you']

def similarity(token1, token2):
    total = len(token1)
    same = 0
    for i in range(len(token1)):
        if token1[i] == token2[i]:
            same += 1
    return float(same)/float(total)

def match(word, equation):
    word = re.sub('['+string.punctuation+']', '', word)
    equation = re.sub('['+string.punctuation+']', '', equation)

    if len(word) > len(equation) or len(word) == 0 or word.isdigit():
        return -1, -1
    if word == '∗':
        return -1, -1
    high = 0.0
    final_offset = -1
    for offset in range(len(equation)-len(word)):
        score = similarity(word, equation[offset:])
        if score > high:
            high = score
            final_offset = offset
    if high > 0.7:
        return final_offset, high
    return -1, high

def var_in_text(db):
    with open('words_alpha.txt') as word_file:
        valid_words = set(word_file.read().split())

    MAX_RANGE = 2
    session = Meta.init(db).Session()
    def get_all_equations():
        return session.query(Equation).order_by(Equation.id)
    def get_sentences_in_doc(doc_id):
        return session.query(Sentence).filter(Sentence.document_id == doc_id)

    for eqt in get_all_equations():
        sentences = get_sentences_in_doc(eqt.document_id)
        #print(sentences.count())
        paragraph_id = eqt.paragraph_id
        #print('*******************************************************')
        count = 0
        id_temp = paragraph_id
        while count < MAX_RANGE:
            id_temp -= 1
            sents = sentences.filter(Sentence.paragraph_id == id_temp)
            if sents.count() == 0:
                count = MAX_RANGE
                break
            if sents[0].name == 'Body Text':
                for sent in sents:
                    for idx, word in enumerate(sent.text.split()):
                        tmp = re.sub('['+string.punctuation+']', '', word)
                        if tmp in stop_words:
                            continue
                        tmp = tmp.lower()
                        if tmp not in valid_words or len(tmp) <= 2:
                            offset, score = match(word, eqt.text)
                            if offset >= 0:
                                #print(str(offset)+' '+str(score)+' '+word)
                                print(sent.page[idx])

                                v = Variable(
                                    text = word,
                                    document_id = eqt.document_id,
                                    equation_id = eqt.id,
                                    equation_text = eqt.text,
                                    equation_offset = offset,
                                    sentence_id = sent.id,
                                    sentence_offset = idx,
                                    sentence_text = sent.text,
                                    score = score,
                                    var_top = sent.top[idx],
                                    var_bottom = sent.bottom[idx],
                                    var_left = sent.left[idx],
                                    var_right = sent.right[idx],
                                    var_page = sent.page[idx]
                                )
                                session.add(v)
                    #print(sent.paragraph_id)
                    #print(sent.text)
                count += 1

        #print('-------------------------------------------------------')
        #print(eqt.text)
        #print('-------------------------------------------------------')
        count = 0
        id_temp = paragraph_id
        while count < MAX_RANGE:
            id_temp += 1
            sents = sentences.filter(Sentence.paragraph_id == id_temp)
            if sents.count() == 0:
                count = MAX_RANGE
                break
            if sents[0].name == 'Body Text':
                for sent in sents:
                    for idx, word in enumerate(sent.text.split()):
                        tmp = re.sub('['+string.punctuation+']', '', word)
                        if tmp in stop_words:
                            continue
                        tmp = tmp.lower()
                        if tmp not in valid_words or len(tmp) <= 2:
                            offset, score = match(word, eqt.text)
                            if offset >= 0:
                                #print(str(offset)+' '+str(score)+' '+word)
                                v = Variable(
                                    text = word,
                                    document_id = eqt.document_id,
                                    equation_id = eqt.id,
                                    equation_text = eqt.text,
                                    equation_offset = offset,
                                    sentence_id = sent.id,
                                    sentence_offset = idx,
                                    sentence_text = sent.text,
                                    score = score,
                                    var_top = sent.top[idx],
                                    var_bottom = sent.bottom[idx],
                                    var_left = sent.left[idx],
                                    var_right = sent.right[idx],
                                    var_page = sent.page[idx]
                                )
                                session.add(v)
                    #print(sent.paragraph_id)
                    #print(sent.text)
                count += 1
        #print('*******************************************************')
        session.commit()

if __name__ == '__main__':
    var_in_text(db_connect_str)
