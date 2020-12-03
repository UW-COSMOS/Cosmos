FROM python:3.8-buster

RUN pip install elasticsearch_dsl requests-aws4auth pandas click pyarrow

COPY cosmos/retrieval /retrieval
WORKDIR /retrieval
RUN pip install .

COPY /cli/load_elastic.sh /cli/load_elastic.sh
RUN chmod +x /cli/load_elastic.sh

CMD /cli/load_elastic.sh

