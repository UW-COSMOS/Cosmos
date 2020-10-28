from enrich import Enrich
import logging

logging.basicConfig()
logging.getLogger().setLevel(logging.INFO)


def enrich_ingest_output(file_path, dataset_id, scispacy_models, output_path, spans, threshold):
    enrich = Enrich(file_path, dataset_id, scispacy_models)
    enrich.semantic_enrichment(output_path,
                               spans=spans,
                               threshold=threshold)


if __name__ == '__main__':
    file_path = '/hdd/iain/covid_output/'
    dataset_id = 'covid_docs_all'
    model_names = ['en_core_sci_md',
                   'en_ner_craft_md',
                   'en_ner_bc5cdr_md',
                   'en_ner_bionlp13cg_md',
                   'en_ner_jnlpba_md']
    output_path = '/hdd/iain/semantic_context.parquet'
    spans = 20
    threshold = 0.9
    enrich_ingest_output(file_path=file_path,
                         dataset_id=dataset_id,
                         scispacy_models=model_names,
                         output_path=output_path,
                         spans=spans,
                         threshold=threshold)

