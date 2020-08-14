time python -m ingest.scripts.ingest_documents --use-semantic-detection \
    --use-xgboost-postprocess -a pdfs -a sections \
    --input-path /hdd/iain/covid_docs_25Mar_all --dataset-id covid_docs_25Mar  \
    --output-path /hdd/iain/covid_output/
