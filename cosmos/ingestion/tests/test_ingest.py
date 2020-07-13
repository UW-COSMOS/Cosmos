from ingest.ingest import Ingest

if __name__ == '__main__':
    ingest = Ingest('tcp://localhost:8786', tmp_dir='tmp', use_semantic_detection=True)
    ingest.ingest('/ssd/ankur/c2', 'contracts', 'contracts.parquet', remove_watermark=False, visualize_proposals=False)
