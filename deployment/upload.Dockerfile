FROM python:3.8-buster

RUN pip install elasticsearch_dsl requests-aws4auth pandas click pyarrow

COPY cosmos/retrieval /retrieval
WORKDIR /retrieval
RUN pip install .

COPY /cli/build_elastic.sh /cli/build_elastic.sh
RUN chmod +x /cli/build_elastic.sh

CMD /cli/build_elastic.sh

