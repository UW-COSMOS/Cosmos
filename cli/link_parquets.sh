#!/bin/bash
python3.8 -m ingest.process.entity_linking.link --input-path /input --output-path /output/ \
    --cluster tcp://scheduluer:8786 --dataset-id documents
