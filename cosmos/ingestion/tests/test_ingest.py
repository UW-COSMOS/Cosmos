from ingest.ingest import Ingest

if __name__ == '__main__':
    ingest = Ingest('tcp://localhost:8786', tmp_dir='/hdd/ankur/tmp', use_semantic_detection=True, use_xgboost_postprocess=True, use_rules_postprocess=False)
    ingest.ingest('/ssd/ankur/c', 'contracts', 'contracts.parquet', visualize_proposals=False)
