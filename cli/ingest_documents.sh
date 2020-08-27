#!/bin/bash
python -m ingest.scripts.ingest_documents --use-semantic-detection \
    --use-xgboost-postprocess -a pdfs -a sections \
    --input-path /input --output-path /output --dataset-id documents \
    --cluster tcp://scheduler:8786 --tmp-dir /mytmp