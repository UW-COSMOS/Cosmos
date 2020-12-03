#!/bin/bash
python3.8 -m retrieval.scripts.delete_elastic_index \
  --host es01 \
  --dataset-id documents
