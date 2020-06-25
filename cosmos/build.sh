#!/bin/bash

# Ingest/process
#docker build -t uwcosmos/aggregate_sections:dev -f aggregate_sections/Dockerfile .
#docker build -t uwcosmos/birdnest_backend:dev -f birdnest_backend/Dockerfile .
#docker build -t uwcosmos/extract_tables:dev -f table_extractions/Dockerfile .
#docker build -t uwcosmos/ingestion:dev -f ingestion/Dockerfile .
#docker build -t uwcosmos/sbackend:dev -f page_api/Dockerfile .
#docker build -t uwcosmos/visualizer:dev -f visualizer/frontend-shared/Dockerfile.build ./visualizer/frontend-shared
docker build -t uwcosmos/request:dev -f scripts/Dockerfile ./scripts


# Retrieval/recall
#docker build -t uwcosmos/retrieval:dev -f retrieval/Dockerfile .
#docker build -t uwcosmos/es_ingest:dev -f elasticsearch/Dockerfile .



#docker build -t iaross/word_embeddings_dev -f word_embeddings/Dockerfile .
#docker build -t iaross/change_dataset_id -f change_dataset_id/Dockerfile .

