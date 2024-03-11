ARG VERSION=latest
FROM uwcosmos/cosmos-base:$VERSION

COPY deployment/weights/lp_genseg_improvement_model_final.pth /weights/lp_genseg_improvement_model_final.pth
COPY deployment/weights/model_weights.pth /weights/model_weights.pth
COPY deployment/weights/pp_model_weights.pth /weights/pp_model_weights.pth
COPY deployment/configs /configs
COPY cli/ingest_documents.sh /cli/ingest_documents.sh
RUN chmod +x /cli/ingest_documents.sh

COPY cosmos/ingestion /ingestion
WORKDIR /ingestion
# TODO this should probably go in cosmos-base, but there are some additional dependency issues to untangle there
RUN apt-get update && apt-get install -y pkg-config && python3.8 -m pip install pdfplumber
RUN python3.8 -m pip install .
ENV PYTHONPATH ".:${PYTHONPATH}"

CMD /cli/ingest_documents.sh
