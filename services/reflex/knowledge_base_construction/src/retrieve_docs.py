"""
"""

import sqlite3
import logging
from drqa import retriever
import os
import click

logger = logging.getLogger()
logger.setLevel(logging.INFO)
fmt = logging.Formatter('%(asctime)s: [ %(message)s ]', '%m/%d/%Y %I:%M:%S %p')
console = logging.StreamHandler()
console.setFormatter(fmt)
logger.addHandler(console)


def retrieve_docs_for_concept(concept, db_dump, tfidf_model, k):
    ranker = retriever.get_class('tfidf')(tfidf_path=tfidf_model)
    conn = sqlite3.connect(db_dump)
    doc_names, doc_scores = ranker.closest_docs(concept, k)
    docs = []
    for name in doc_names:
        c = conn.cursor()
        c.execute("SELECT * FROM documents WHERE id=?", (name,))
        row = c.fetchone()
        doc = row[1]
        docs.append(doc)
    return docs

def retrieve_docs(concept, ranker, conn, k):
    doc_names, doc_scores = ranker.closest_docs(concept, k)
    docs = []
    for name in doc_names:
        c = conn.cursor()
        c.execute("SELECT * FROM documents WHERE id=?", (name,))
        row = c.fetchone()
        doc = row[1]
        docs.append(doc)
    return docs


def retrieve_doc_set(db_dump, concept_file, tfidf_model, k, output):
    ranker = retriever.get_class('tfidf')(tfidf_path=tfidf_model)
    conn = sqlite3.connect(db_dump)
    with open(concept_file, 'r') as cf, open(output, 'w') as of:
        for line in cf:
            line = line.strip()
            doc_names, doc_scores = ranker.closest_docs(line, k)
            print(doc_names)
            for name in doc_names:
                print(name)
                c = conn.cursor()
                c.execute("SELECT * FROM documents where id=?", (name,))
                row = c.fetchone()
                print(row)
                doc = row[1]
                of.write(f'{doc}\n')

@click.command()
@click.option('--db-dump')
@click.option('--concept-file')
@click.option('--tfidf-model')
@click.option('--k-closest-docs')
@click.option('--output-file')
def retrieve(db_dump, concept_file, tfidf_model, k_closest_docs, output_file):
    retrieve_doc_set(db_dump, concept_file, tfidf_model, int(k_closest_docs), output_file)


if __name__ == '__main__':
    retrieve()

    

