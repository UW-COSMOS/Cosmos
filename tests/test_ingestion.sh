# Assume that we already have the ingestion image built
#docker run -v /ssd/ankur/Cosmos/tests/one_pdf:/test -v /ssd/ankur/Cosmos/tests/output:/output--network="swarm_network" ${INGEST_IMAGE} "python -m ingest.scripts.ingest_documents --use-semantic-detection \
#    --use-xgboost-postprocess -a pdfs -a sections \
#    --input-path /test --output-path /output --dataset-id one_pdf"

docker run -v /ssd/ankur/Cosmos/tests/one_pdf:/test -v /ssd/ankur/Cosmos/tests/output:/output--network="swarm_network" ${INGEST_IMAGE} "which python"

