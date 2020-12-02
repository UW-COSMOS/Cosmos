FROM uwcosmos/cosmos-base:latest

RUN python3.8 -m pip install scispacy spacy==2.3.2
RUN python3.8 -m pip install https://s3-us-west-2.amazonaws.com/ai2-s2-scispacy/releases/v0.3.0/en_core_sci_lg-0.3.0.tar.gz

COPY cosmos/ingestion /ingestion
WORKDIR /ingestion
RUN python3.8 -m pip install .
ENV PYTHONPATH ".:${PYTHONPATH}"

RUN python3.8 -m ingest.scripts.load_linking_files

COPY cli/link_parquets.sh /cli/link_parquets.sh
RUN chmod +x /cli/link_parquets.sh

CMD /cli/link_parquets.sh

