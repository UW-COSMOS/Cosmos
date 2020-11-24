from enrich import run_enrich
import logging

logging.basicConfig()
logging.getLogger().setLevel(logging.INFO)

if __name__ == '__main__':
    input_path = '/hdd/iain/covid_output'
    output_path = '/hdd/iain/context_enriched_output'
    dataset_id = 'covid_docs_all'

    spans = 20
    threshold = 0.8
    run_enrich(input_path, output_path, dataset_id, spans, threshold)
