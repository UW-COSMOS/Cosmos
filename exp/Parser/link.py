import argparse
from fonduer import Meta
from fonduer.parser.models import Document, Sentence
import json
from itertools import chain
from collections import defaultdict
import loguru


def link(words_location, db_connect_str, ignored_files=[]):
    session = Meta.init(db_connect_str).Session()

    def get_word_bag(html_source):
        with open(words_location + html_source + '.html.json', encoding='utf-8') as words:
            return json.load(words)

    def get_all_documents():
        return session.query(Document).order_by(Document.id)

    def get_all_sentence_from_a_doc(doc_id):
        return session.query(Sentence).filter(Sentence.document_id == doc_id).order_by(Sentence.id)

    def same(w1, w2):
        return w1.replace('-', '—') == w2.replace('-', '—')

    for doc in get_all_documents():
        if doc.name + '.html' in ignored_files:
            continue
        word_bag = get_word_bag(doc.name)
        sentences = get_all_sentence_from_a_doc(doc.id)
        word_bag_count = 0
        all_words_from_db = list(chain(*[sent.text.split() for sent in sentences]))

        assert len(all_words_from_db) >= len(word_bag)
        open('db_words.txt', 'w', encoding='utf-8').write('\n'.join(all_words_from_db))
        open('json_words.txt', 'w',encoding='utf-8').write('\n'.join(map(lambda x: x['text'], word_bag)))
        str_buffer = ''
        for sent in sentences:
            coordinates_record = defaultdict(list)
            tokenized_words = sent.text.split()

            def add_to_coordinate_record_list(current_idx_json):
                current_word_from_bag = word_bag[current_idx_json]
                coordinates_record['top'].append(current_word_from_bag['line_bbox']['ymin'])
                coordinates_record['left'].append(current_word_from_bag['word_bbox']['xmin'])
                coordinates_record['bottom'].append(current_word_from_bag['line_bbox']['ymax'])
                coordinates_record['right'].append(current_word_from_bag['word_bbox']['xmax'])
                coordinates_record['page_num'].append(current_word_from_bag['word_bbox']['page_num'])

            for word in tokenized_words:
                add_to_coordinate_record_list(word_bag_count)
                if same(word, word_bag[word_bag_count]['text']):
                    word_bag_count += 1
                else:
                    str_buffer += word
                    if same(str_buffer, word_bag[word_bag_count]['text']):
                        # loguru.logger.debug("%s : %s" % (str_buffer, word_bag[word_bag_count]['text']))
                        str_buffer = ''
                        word_bag_count += 1

            sent.top = coordinates_record['top']
            sent.left = coordinates_record['left']
            sent.bottom = coordinates_record['bottom']
            sent.right = coordinates_record['right']
            sent.page = coordinates_record['page_num']

            def sanity_check():
                try:
                    assert len(sent.text.split()) == len(sent.top) == len(sent.left) == len(sent.right) == len(
                        sent.bottom) == len(sent.page)
                except AssertionError:
                    print(len(sent.text.split()), len(sent.top), len(sent.left), len(sent.bottom), len(sent.right),
                          len(sent.page))
                    assert False

            sanity_check()

    session.commit()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--words_location', help='location of word coordinate JSON file', default='out/words/')
    parser.add_argument('--database', help='database connection string', default='postgres://postgres:password@localhost:5432/cosmos')
    parser.add_argument('--ignored_files', help='files to be ignored', nargs='+', default=[])
    args = parser.parse_args()
    link(args.words_location, args.database, args.ignored_files)
