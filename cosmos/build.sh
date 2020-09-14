#!/bin/bash

# Ingest/process
docker build -t uwcosmos/ingestion -f ingestion/Dockerfile .
docker build -t uwcosmos/sbackend -f page_api/Dockerfile .
docker build -t uwcosmos/visualizer -f visualizer/frontend-shared/Dockerfile.build ./visualizer/frontend-shared
docker build -t uwcosmos/request -f scripts/Dockerfile ./scripts
docker build -t uwcosmos/schema -f database/Dockerfile ./database

# extract/aggregate
docker build -t uwcosmos/extract_tables -f table_extractions/Dockerfile .
docker build -t uwcosmos/aggregate_sections -f aggregate_sections/Dockerfile .

# Retrieval/recall
docker build -t uwcosmos/retrieval -f retrieval/Dockerfile .
docker build -t uwcosmos/birdnest_backend -f birdnest_backend/Dockerfile .
docker build -t uwcosmos/es_ingest -f elasticsearch/Dockerfile .

# Other.
#docker build -t iaross/word_embeddings_dev -f word_embeddings/Dockerfile .
#docker build -t iaross/change_dataset_id -f change_dataset_id/Dockerfile .

