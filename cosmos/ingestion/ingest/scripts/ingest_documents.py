import click
from ingest.ingest import Ingest
from ingest.utils.table_extraction import TableLocationProcessor
import os


@click.command()
@click.option('--cluster', type=str, default='tcp://localhost:8786', help='dask cluster tcp address')
@click.option('--tmp-dir', type=str, default='tmp', help='set temp dir')
@click.option('--use-semantic-detection/--no-semantic-detection', type=bool, default='True', help='enable or disable semantic detection')
@click.option('--use-xgboost-postprocess/--no-xgboost-postprocess', type=bool, default='True', help='enable or disable xgboost postprocess')
@click.option('--use-rules-postprocess/--no-rules-postprocess', type=bool, default='False', help='enable or disable rules postprocess')
@click.option('--use-table-context-enrichment/--no-table-context-enrichment', type=bool, default='False', help='add body text context to table text based on table corefences in text')
@click.option('--use-qa-table-enrichment/--no-qa-table-enrichment', type=bool, default='False', help='output table detection statistics based on table corefences in text')
@click.option('--use-text-normalization/--no-text-normalization', type=bool, default='False', help='normalize text (ligature/unicode cleanup)')
@click.option('--aggregation', '-a', multiple=True, default=[])
@click.option('--input-path', type=click.Path(exists=True), help='define the path to your input documents')
@click.option('--dataset-id', type=str, default='cosmos', help='dataset id')
@click.option('--output-path', type=click.Path(), default='./', help='define a path for output')
@click.option('--visualize-proposals/--no-visualize-proposals', type=bool, default='False', help='enable or disable proposal viz')
@click.option('--skip-ocr/--no-skip-ocr', type=bool, default='True', help='Use OCR over documents with no metadata. Requires Tesseract v4 installed on system.')
@click.option('--extract-tables/--no-extract-tables', type=bool, default='False', help='Extract a dataframe representation of identified table objects.')
@click.option('--compute-word-vecs/--no-compute-word-vecs', type=bool, default='False', help='Compute word vectors')
@click.option('--ngram', type=int, default=3, help='ngram for computing word vecs')
@click.option('--pp_threshold', type=float, default=0.8, help='postprocess_score threshold for identifying an object for table context enrichment')
@click.option('--d_threshold', type=float, default=-10, help='detect_score threshold for identifying an object for table context enrichment')
@click.option('--spans', type=int, default=20, help='number of words either side of a table coreference to capture for context')
def ingest_documents(cluster,
                     tmp_dir,
                     use_semantic_detection,
                     use_xgboost_postprocess,
                     use_rules_postprocess,
                     use_table_context_enrichment,
                     use_qa_table_enrichment,
                     use_text_normalization,
                     aggregation,
                     input_path,
                     dataset_id,
                     output_path,
                     visualize_proposals,
                     skip_ocr,
                     extract_tables,
                     compute_word_vecs,
                     ngram,
                     pp_threshold,
                     d_threshold,
                     spans):
    ingest = Ingest(cluster,
                    tmp_dir=tmp_dir,
                    use_semantic_detection=use_semantic_detection,
                    use_xgboost_postprocess=use_xgboost_postprocess,
                    use_rules_postprocess=use_rules_postprocess,
                    use_table_context_enrichment=use_table_context_enrichment,
                    use_qa_table_enrichment=use_qa_table_enrichment,
                    use_text_normalization=use_text_normalization)
    ingest.ingest(input_path,
                  dataset_id,
                  output_path,
                  os.path.join(output_path, 'images'),
                  skip_ocr=skip_ocr,
                  visualize_proposals=visualize_proposals,
                  aggregations=aggregation,
                  compute_word_vecs=compute_word_vecs,
                  ngram=ngram,
                  pp_threshold=pp_threshold,
                  d_threshold=d_threshold,
                  spans=spans)
    if extract_tables:
        proc = TableLocationProcessor(
                os.path.join(output_path, f"{dataset_id}_tables.parquet"),
                str(input_path) + os.sep,
                "",
                os.path.join(output_path, 'tables/')
                )
        _ = proc.extract_pickles()


if __name__ == '__main__':
    ingest_documents()
