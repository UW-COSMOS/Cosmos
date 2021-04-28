from ingest.ingest import Ingest
from pathlib import Path

cluster = 'tcp://localhost:8786'
tmp_dir = '/hdd/iain/enrichment_testing/tmp'
use_semantic_detection = True
use_xgboost_postprocess = True
use_rules_postprocess = False

dataset_id = 'documents_5Feb'
path_to_parquet_dirs = '/hdd/iain/enrichment_testing/cosmos_covid_5Feb2021_enriched'
pp_threshold = 0.72
d_threshold = -10
spans = 20

if __name__ == '__main__':
    ingest = Ingest(cluster,
                    tmp_dir=tmp_dir,
                    use_semantic_detection=use_semantic_detection,
                    use_xgboost_postprocess=use_xgboost_postprocess,
                    use_rules_postprocess=use_rules_postprocess)

    for enrichment_path in Path(path_to_parquet_dirs).glob('*'):
        ingest.enrich(file_path=enrichment_path,
                      dataset_id=dataset_id,
                      pp_threshold=pp_threshold,
                      d_threshold=d_threshold,
                      spans=spans)
