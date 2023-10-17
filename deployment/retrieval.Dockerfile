ARG VERSION=latest
FROM uwcosmos/cosmos-base:$VERSION

COPY deployment/weights/bert_reranker /weights/bert_reranker

COPY cosmos/retrieval /retrieval
WORKDIR /retrieval
RUN python3.8 -m pip install .

