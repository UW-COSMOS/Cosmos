FROM uwcosmos/cosmos-ingestion:latest

WORKDIR /src/
RUN pip3.8 install fastapi[all]==0.99.0 rocketry
COPY cosmos_service/src/ /src/
COPY htcosmos/make_parquet.py /src/util/

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8089"]
