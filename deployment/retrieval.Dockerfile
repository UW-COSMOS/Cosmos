FROM ankurgos/cosmos-base:latest

COPY deployment/weights/bert_reranker /weights/bert_reranker

COPY cosmos/retrieval /retrieval
WORKDIR /retrieval
RUN pip install .

