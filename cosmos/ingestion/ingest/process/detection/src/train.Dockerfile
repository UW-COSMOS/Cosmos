FROM ankurgos/ingest:3.0 

RUN mkdir /app
WORKDIR /app
COPY ingestion/ingest/process/detection/src/requirements.txt /app
RUN pip install -r requirements.txt --user
COPY ingestion/ingest/process/detection/src /app

