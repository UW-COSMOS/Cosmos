FROM python:3.8-buster

RUN pip install elasticsearch_dsl requests-aws4auth pandas click pyarrow

COPY cosmos/retrieval /retrieval
WORKDIR /retrieval
RUN pip install .

