#!/bin/bash
time python -m ingest.scripts.ingest_documents --use-semantic-detection \
    --use-xgboost-postprocess -a pdfs -a sections \
    --input-path /hdd/iain/covid_docs_25Mar_all --dataset-id covid_docs_25Mar  \
    --output-path /hdd/iain/covid_output/ --tmp-dir /ssd/iain/mytmp \
    --cluster tcp://128.104.100.153:8786
