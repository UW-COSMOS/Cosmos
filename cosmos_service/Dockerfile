FROM uwcosmos/cosmos-ingestion

WORKDIR /src/
RUN pip3.8 install fastapi[all]
RUN pip3.8 install rocketry
COPY src/ /src/

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8089"]
