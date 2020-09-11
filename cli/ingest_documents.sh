#!/bin/bash
python -m ingest.scripts.ingest_documents --use-semantic-detection \
    --use-xgboost-postprocess -a pdfs -a sections -a tables -a figures -a equations \
    --compute-word-vecs  --ngram 3 \
    --input-path /input --output-path /output --dataset-id documents \
    --cluster tcp://scheduler:8786 --tmp-dir /mytmp