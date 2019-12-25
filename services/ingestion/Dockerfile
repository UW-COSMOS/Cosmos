FROM python:3.7


RUN apt-get update
# Ghostscript
RUN apt-get install -y ghostscript gcc default-libmysqlclient-dev

COPY ingestion/requirements.txt /
RUN pip install -r /requirements.txt


# Copy test documents to /test

#COPY tests/pdfs /test

COPY ingestion/src/ /app
COPY database/schema.py /app

WORKDIR /
