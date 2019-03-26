import os
import sys
sys.path.append(os.path.dirname(__file__))
import loguru
from collections import defaultdict
from itertools import chain
import argparse
from fonduer import Meta
from fonduer.parser.models import Document, Sentence
import json

from fonduer.meta import Meta as Mt
from sqlalchemy import Column, Integer, String, Text, ForeignKey
from sqlalchemy.dialects import postgresql
from os.path import join

import re


STR_ARRAY_TYPE = postgresql.ARRAY(String)
_meta = Mt.init()

class Latex(_meta.Base):
    """Latex representation of sentences"""
    __tablename__ = "latexsentence"

    id = Column(Integer, primary_key=True)

    name = Column(String, unique=False, nullable=True)

    #: The id of the parent ``Document``.
    document_id = Column(Integer)

    #: The id of the parent ``Section``.
    section_id = Column(Integer)
    #: The parent ``Section``.

    #: The id of the parent ``Paragraph``.
    paragraph_id = Column(Integer)

    #: The full text of the ``Sentence``.
    text = Column(Text, nullable=False)

    #: A list of the words in a ``Sentence``.
    tokens = Column(STR_ARRAY_TYPE)

class Image(_meta.Base):
    """Path to the image of the original pdf for each section"""
    __tablename__ = "image"
    id = Column(Integer, primary_key=True)
    img_path = Column(Text, nullable=False)


def link(words_location, db_connect_str, ignored_files=[]): 
    """
    Put the coordinate information for each token into the db and get the path to the original pdf image for each section.
    :param words_location: Directory storing the json files which contain the word coordinate information.
    :param db_connect_str: db connection string.
    :param ignored_files: Files to be ignored.
    """
    XPATH_PATTERN = re.compile("/html/body/div\[([0-9]+)\]/div\[([0-9]+)\]/div\[.*\]/p.*")
    session = Meta.init(db_connect_str).Session()

    def get_word_bag(html_source):
        with open(words_location + html_source + '.html.json', encoding='utf-8') as words:
            return list(filter(lambda x: x['type'] != 'Equation', json.load(words)))
    def get_img_path(html_source):
        with open(words_location + 'path_info_' +html_source + '.html.json', encoding='utf-8') as paths:
            return json.load(paths)

    def get_all_documents():
        return session.query(Document).order_by(Document.id)

    def get_all_sentence_from_a_doc(doc_id):
        return session.query(Sentence).filter(Sentence.document_id == doc_id, Sentence.name != 'Equation').order_by(Sentence.id)

    def get_all_sentence_with_equation(doc_id):
        return session.query(Sentence).filter(Sentence.document_id == doc_id).order_by(Sentence.paragraph_id)

    def same(w1, w2):
        hyphens = ['\u2010','\u2011','\u2012','\u2013','\u2014','\u2212']
        for hyphen in hyphens:
            w1 = w1.replace(hyphen,'-')
            w2 = w2.replace(hyphen,'-')
        return w1 == w2

    for doc in get_all_documents():
        if doc.name + '.html' in ignored_files:
            continue
        word_bag = get_word_bag(doc.name)
        paths = get_img_path(doc.name)
        #for path in paths:
            #print(path)
        sentences = get_all_sentence_from_a_doc(doc.id)
        sentences_with_eq = get_all_sentence_with_equation(doc.id)
        word_bag_count = 0
        path_count = -1
        all_words_from_db = list(
            chain(*[sent.text.split() for sent in sentences]))

        #loguru.logger.debug(len(all_words_from_db))
        #loguru.logger.debug(len(word_bag))
        #open('db_words.txt', 'w', encoding='utf-8').write('\n'.join(all_words_from_db))
        #open('json_words.txt', 'w',
             #encoding='utf-8').write('\n'.join(map(lambda x: x['text'], word_bag)))
        assert len(all_words_from_db) >= len(word_bag)

        str_buffer = ''
        index1_prev = 'nonesense'
        index2_prev = 'nonesense'

        for sent in sentences_with_eq:
            xpath = sent.xpath
            match = XPATH_PATTERN.search(xpath)
            if match is None:
                continue
            index1 = match.group(1)
            index2 = match.group(2)

            if index1 == index1_prev and index2 == index2_prev:
                #print('the same')
                #print(path_count)
                #print(paths[path_count])
                img_p = Image(
                    id = sent.id,
                    img_path = paths[path_count]
                )
                session.add(img_p)
            else:
                path_count += 1
                if path_count >= len(paths):
                    print("There is no path available")
                else:
                    #print('different')
                    #print(path_count)
                    #print(paths[path_count])
                    img_p = Image(
                        id = sent.id,
                        img_path = paths[path_count]
                    )
                    session.add(img_p)
                    index1_prev = index1
                    index2_prev = index2


        for sent in sentences:
            coordinates_record = defaultdict(list)
            tokenized_words = sent.text.split()

            latex_tokens = []

            def add_to_coordinate_record_list(current_idx_json):
                current_word_from_bag = word_bag[current_idx_json]
                coordinates_record['top'].append(
                    current_word_from_bag['line_bbox']['ymin'])
                coordinates_record['left'].append(
                    current_word_from_bag['word_bbox']['xmin'])
                coordinates_record['bottom'].append(
                    current_word_from_bag['line_bbox']['ymax'])
                coordinates_record['right'].append(
                    current_word_from_bag['word_bbox']['xmax'])
                coordinates_record['page_num'].append(
                    current_word_from_bag['word_bbox']['page_num'])

            for word in tokenized_words:
                add_to_coordinate_record_list(word_bag_count)
                latex_tokens.append(word_bag[word_bag_count]['latex'])
                if same(word, word_bag[word_bag_count]['text'].replace(' ','')):
                    word_bag_count += 1
                else:
                    str_buffer += word
                    #print('*********'+str_buffer)
                    #print(word_bag[word_bag_count]['text'].replace(' ',''))
                    if same(str_buffer, word_bag[word_bag_count]['text'].replace(' ','')):
                        # loguru.logger.debug("%s : %s" % (str_buffer, word_bag[word_bag_count]['text']))
                        str_buffer = ''
                        word_bag_count += 1

            l = Latex(
                document_id = sent.document_id,
                name = sent.name,
                section_id = sent.section_id,
                paragraph_id = sent.paragraph_id,
                text = sent.text,
                tokens = latex_tokens
            )

            session.add(l)

            sent.top = coordinates_record['top']
            sent.left = coordinates_record['left']
            sent.bottom = coordinates_record['bottom']
            sent.right = coordinates_record['right']
            sent.page = coordinates_record['page_num']
            # print(coordinates_record['page_num'])

            def sanity_check():
                try:
                    assert len(sent.text.split()) == len(sent.top) == len(sent.left) == len(sent.right) == len(
                        sent.bottom) == len(sent.page)
                except AssertionError:
                    print(len(sent.text.split()), len(sent.top), len(sent.left), len(sent.bottom), len(sent.right),
                          len(sent.page))
                    assert False

            if sent.name != 'Equation':
                sanity_check()
    print('Before commit')
    session.commit()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--words_location', help='location of word coordinate JSON file', default='out/words/')
    parser.add_argument('--database', help='database connection string', default='postgres://postgres:password@localhost:5432/cosmos')
    parser.add_argument('--ignored_files', help='files to be ignored', nargs='+', default=[])
    args = parser.parse_args()
    link(args.words_location, args.database, args.ignored_files)
