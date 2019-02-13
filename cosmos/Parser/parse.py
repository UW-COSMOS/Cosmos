from fonduer import Meta
from fonduer.parser.preprocessors import HTMLDocPreprocessor
from fonduer.parser import Parser
import argparse

def parse(html_location, database):
    session = Meta.init(database).Session()
    doc_preprocessor = HTMLDocPreprocessor(html_location)
    corpus_parser = Parser(session, structural=True, lingual=True)
    corpus_parser.apply(doc_preprocessor)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--html_location', default='out/html/')
    parser.add_argument('--database', default='postgres://postgres:password@localhost:5432/cosmos')
    args = parser.parse_args()
    parse(args.html_location, args.database)
