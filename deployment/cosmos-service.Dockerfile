ARG VERSION=latest
FROM uwcosmos/cosmos-ingestion:$VERSION

ARG VERSION
ARG GIT_HASH
ENV API_VERSION=$VERSION
ENV GIT_HASH=$GIT_HASH

WORKDIR /src/
RUN pip3.8 install fastapi[all]==0.99.0 rocketry
COPY cosmos_service/src/ /src/
COPY htcosmos/make_parquet.py /src/util/

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8089"]
