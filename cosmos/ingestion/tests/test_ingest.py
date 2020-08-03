from ingest.ingest import Ingest

if __name__ == '__main__':
    ingest = Ingest('tcp://localhost:8786', tmp_dir='/hdd/ankur/tmp', use_semantic_detection=True, use_xgboost_postprocess=True, use_rules_postprocess=True)
    ingest.ingest('/hdd/ankur/covid2', 'covid2', 'covid2.parquet', visualize_proposals=True)
