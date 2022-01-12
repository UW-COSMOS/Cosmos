FROM iaross/cosmos-base:dev

COPY deployment/weights/model_weights.pth /weights/model_weights.pth
COPY deployment/weights/pp_model_weights.pth /weights/pp_model_weights.pth
COPY deployment/configs /configs
COPY cli/ingest_documents.sh /cli/ingest_documents.sh
RUN chmod +x /cli/ingest_documents.sh

COPY cosmos/ingestion /ingestion
WORKDIR /ingestion
RUN python3.8 -m pip install .
ENV PYTHONPATH ".:${PYTHONPATH}"

CMD /cli/ingest_documents.sh
