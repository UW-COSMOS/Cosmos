ARG VERSION=latest
FROM uwcosmos/cosmos-ingestion:$VERSION

ARG VERSION
ARG GIT_HASH
ENV API_VERSION=$VERSION
ENV GIT_HASH=$GIT_HASH

ENV MODEL_CONFIG=/configs/model_config.yaml
ENV WEIGHTS_PTH=/weights/model_weights.pth
ENV PP_WEIGHTS_PTH=/weights/pp_model_weights.pth

WORKDIR /src/
RUN mkdir /pdfs /pages /output
COPY cosmos_service/src/ /src/
ENV AGGREGATIONS=pdfs,sections,tables,figures,equations
COPY htcosmos/make_parquet.py /src/util/
CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8089"]
