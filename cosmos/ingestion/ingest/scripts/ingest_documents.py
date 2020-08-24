import click
from ingest.ingest import Ingest
import os


@click.command()
@click.option('--cluster', type=str, default='tcp://localhost:8786', help='dask cluster tcp address')
@click.option('--tmp-dir', type=str, default='tmp', help='set temp dir')
@click.option('--use-semantic-detection/--no-semantic-detection', type=bool, default='True', help='enable or disable semantic detection')
@click.option('--use-xgboost-postprocess/--no-xgboost-postprocess', type=bool, default='True', help='enable or disable xgboost postprocess')
@click.option('--use-rules-postprocess/--no-rules-postprocess', type=bool, default='False', help='enable or disable rules postprocess')
@click.option('--aggregation', '-a', multiple=True, default=[])
@click.option('--input-path', type=click.Path(exists=True), help='define the path to your input documents')
@click.option('--dataset-id', type=str, default='cosmos', help='dataset id')
@click.option('--output-path', type=click.Path(), default='./', help='define a path for output')
@click.option('--visualize-proposals/--no-visualize-proposals', type=bool, default='False', help='enable or disable proposal viz')
@click.option('--skip-ocr/--no-skip-ocr', type=bool, default='True',
              help='Use OCR over documents with no metadata. Requires Tesseract v4 installed on system.')
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
                     skip_ocr):
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
                  aggregations=aggregation)


if __name__ == '__main__':
    ingest_documents()
