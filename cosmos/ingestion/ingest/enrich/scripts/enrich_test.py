import click
from enrich import Enrich
import logging

logging.basicConfig()
logging.getLogger().setLevel(logging.INFO)

cluster = 'tcp://localhost:8786'
input_path = '/hdd/iain/covid_output'  # '/hdd/iain/covid_output'
output_path = '/hdd/iain/context_enriched_output'
dataset_id = 'covid_docs_all'
spans = 20
threshold = 0.8

# @click.command()
# @click.option('--cluster', type=str, default='tcp://localhost:8786', help='dask cluster tcp address')
# @click.option('--input-path', type=click.Path(exists=True), help='define the path to your input documents')
# @click.option('--output-path', type=click.Path(), default='./', help='define a path for output')
# @click.option('--dataset-id', type=str, default='cosmos', help='dataset id')
# @click.option('--spans', type=int, default=20, help='length of context in words around an object label per direction to fetch')
# @click.option('--threshold', type=float, default=0.8, help='postprocess score required to recognise an object')
def enrich_documents(cluster,
                     input_path,
                     output_path,
                     dataset_id,
                     spans,
                     threshold):
    er = Enrich(cluster)
    er.enrich(input_path=input_path,
              output_path=output_path,
              dataset_id=dataset_id,
              spans=spans,
              threshold=threshold)


if __name__ == '__main__':
    enrich_documents(cluster, input_path, output_path, dataset_id, spans, threshold)
