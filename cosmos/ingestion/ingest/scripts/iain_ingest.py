import click
from ingest.ingest import Ingest
import sys

sys.path.append(r'/ssd/iain/Cosmos/cosmos/ingestion/ingest')

@click.command()
@click.option('--cluster', type=str, default='tcp://localhost:8786', help='dask cluster tcp address')
@click.option('--tmp-dir', type=str, default='tmp', help='set temp dir')
@click.option('--use-semantic-detection', type=bool, default='True', help='enable or disable semantic detection')
@click.option('--input-path', type=click.Path(exists=True), help='define the path to your input documents')
@click.option('--dataset-id', type=str, default='dataset_id', help='dataset id')
@click.option('--output-path', type=click.Path(), default='output.parquet', help='define a path for output')
# @click.option('--remove-watermark', type=bool, default='False', help='enable or disable watermark removal')
@click.option('--visualize-proposals', type=bool, default='False', help='enable or disable proposal viz')
def test_ingestion(cluster,
                    tmp_dir,
                    use_semantic_detection,
                    input_path,
                    dataset_id,
                    output_path,
#                    remove_watermark,
                    visualize_proposals):
    ingest = Ingest(cluster,
                    tmp_dir=tmp_dir,
                    use_semantic_detection=use_semantic_detection)
    ingest.ingest(input_path,
                dataset_id,
                output_path,
#                remove_watermark=remove_watermark,
                visualize_proposals=visualize_proposals)


if __name__ == '__main__':
    test_ingestion()

