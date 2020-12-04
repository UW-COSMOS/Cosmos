#!/bin/bash
python3.8 -m ingest.scripts.ingest_documents --use-semantic-detection \
    --use-xgboost-postprocess -a pdfs -a sections -a tables -a figures -a equations \
    --compute-word-vecs --ngrams 3 \
    --no-enrich \
    --input-path /input --output-path /output --dataset-id documents \
    --cluster tcp://scheduler:8786 --tmp-dir /mytmp
