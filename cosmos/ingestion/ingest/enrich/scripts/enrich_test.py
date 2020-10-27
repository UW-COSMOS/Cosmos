from enrich import Enrich
import logging

logging.basicConfig()
logging.getLogger().setLevel(logging.DEBUG)

file_path = '/hdd/iain/covid_output/'
dataset_id = 'covid_docs_all'


def enrich_ingest_output(file_path, dataset_id, spacy_model):
    enrich = Enrich(file_path, dataset_id, spacy_model)
    enrich.semantic_enrichment('/hdd/iain/semantic_context.parquet',
                               spans=20,
                               threshold=0.9)


if __name__ == '__main__':
    enrich_ingest_output(file_path=file_path,
                         dataset_id=dataset_id,
                         spacy_model='en_core_web_md')

