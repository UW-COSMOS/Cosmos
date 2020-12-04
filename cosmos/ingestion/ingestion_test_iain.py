# import click
from ingest.ingest import Ingest
import os

# @click.command()
# @click.option('--cluster', type=str, default='tcp://localhost:8786', help='dask cluster tcp address')
# @click.option('--tmp-dir', type=str, default='tmp', help='set temp dir')
# @click.option('--use-semantic-detection/--no-semantic-detection', type=bool, default='True', help='enable or disable semantic detection')
# @click.option('--use-xgboost-postprocess/--no-xgboost-postprocess', type=bool, default='True', help='enable or disable xgboost postprocess')
# @click.option('--use-rules-postprocess/--no-rules-postprocess', type=bool, default='False', help='enable or disable rules postprocess')
# @click.option('--aggregation', '-a', multiple=True, default=[])
# @click.option('--input-path', type=click.Path(exists=True), help='define the path to your input documents')
# @click.option('--dataset-id', typcone=str, default='cosmos', help='dataset id')
# @click.option('--output-path', type=click.Path(), default='./', help='define a path for output')
# @click.option('--visualize-proposals/--no-visualize-proposals', type=bool, default='False', help='enable or disable proposal viz')
# @click.option('--skip-ocr/--no-skip-ocr', type=bool, default='True',
#               help='Use OCR over documents with no metadata. Requires Tesseract v4 installed on system.')
# @click.option('--enrich/--no-enrich', type=bool, default = False, help='enable or disable context enrichment')
# @click.option('--spans', type=int, default= 20, help='requires --enrich option, set length of words to gather from each side of a coreference in a document')
# @click.option('--threshold', type=float, default = 0.8, help='postprocess_cls cut off for recognising a class to pull context')

# time python -m ingest.scripts.ingest_documents --use-semantic-detection \
#     --use-xgboost-postprocess -a pdfs -a sections \
#     --input-path /hdd/iain/covid_docs_25Mar_all --dataset-id covid_docs_25Mar  \
#     --output-path /hdd/iain/covid_output/ --tmp-dir /ssd/iain/mytmp \
#     --cluster tcp://128.104.100.153:8786

import logging

logging.basicConfig()
logging.getLogger().setLevel(logging.DEBUG)

cluster = 'tcp://localhost:8786'
tmp_dir = '/ssd/iain/enrichment_test_tmp'
use_semantic_detection = True
use_xgboost_postprocess = True
use_rules_postprocess = False
aggregation = ['pdfs', 'sections']
input_path = '/hdd/iain/enrichment_testing_doc'
dataset_id = 'covid_docs_all'
output_path = '/hdd/iain/context_enriched_output/'
visualize_proposals = False
skip_ocr = True
enrich = True
spans = 20
threshold = 0.1


def ingest_documents(cluster,
                     tmp_dir,
                     use_semantic_detection,
                     use_xgboost_postprocess,
                     use_rules_postprocess,
                     aggregation,
                     input_path,
                     dataset_id,
                     output_path,
                     visualize_proposals,
                     skip_ocr,
                     enrich,
                     threshold,
                     spans):
    ingest = Ingest(cluster,
                    tmp_dir=tmp_dir,
                    use_semantic_detection=use_semantic_detection,
                    use_xgboost_postprocess=use_xgboost_postprocess,
                    use_rules_postprocess=use_rules_postprocess)
    ingest.ingest(input_path,
                  dataset_id,
                  output_path,
                  os.path.join(output_path, 'images'),
                  skip_ocr=skip_ocr,
                  visualize_proposals=visualize_proposals,
                  aggregations=aggregation,
                  enrich=enrich,
                  spans=spans,
                  threshold=threshold)


if __name__ == "__main__":
    ingest_documents(cluster=cluster,
                     tmp_dir=tmp_dir,
                     use_semantic_detection=use_semantic_detection,
                     use_xgboost_postprocess=use_xgboost_postprocess,
                     use_rules_postprocess=use_rules_postprocess,
                     aggregation=aggregation,
                     input_path=input_path,
                     dataset_id=dataset_id,
                     output_path=output_path,
                     visualize_proposals=visualize_proposals,
                     skip_ocr=skip_ocr,
                     enrich=enrich,
                     spans=spans,
                     threshold=threshold)
