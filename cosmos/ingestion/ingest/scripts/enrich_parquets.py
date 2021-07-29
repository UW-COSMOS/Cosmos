from ingest.ingest import Ingest
from pathlib import Path

cluster = 'tcp://localhost:8786'
# tmp_dir = '/hdd/iain/enrichment_testing/tmp'
# tmp_dir = '/hdd/iain/enrichment_testing/tmp_100_no_preprints'
# tmp_dir = '/hdd/iain/enrichment_testing/smoke_test_tmp'
tmp_dir = '/hdd/iain/enrichment_testing/dolomites_tmp'

use_semantic_detection = True
use_xgboost_postprocess = True
use_rules_postprocess = False

# dataset_id = 'documents_5Feb'
# dataset_id = 'no_preprints'
dataset_id = 'dolomites'
# path_to_parquet_dirs = '/hdd/iain/enrichment_testing/cosmos_covid_5Feb2021_enriched'
# parquets of 100 no-preprint covid docs (cosmos output): /hdd/iain/enrichment_testing/100_no_preprints_output
# copy those over to '/hdd/iain/enrichment_testing/100_noq_preprints/'
# path_to_parquet_dirs = '/hdd/iain/enrichment_testing/100_no_preprints_enriched'
# path_to_parquet_dirs = '/hdd/iain/enrichment_testing/smoke_test_one_doc'
path_to_parquet_dirs = '/hdd/iain/enrichment_testing/dolomites_docs/'

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
                      spans=spans,
                      qa=True)
