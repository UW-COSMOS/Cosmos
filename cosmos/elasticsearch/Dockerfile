FROM python:3.7-alpine

# Install bash and vim
#RUN apk add bash vim py-mysqldb
RUN apk --no-cache add build-base mariadb-dev

# Pillow requirements
#run apt install build-base jpeg-dev zlib-dev

RUN pip install elasticsearch_dsl pymongo click

COPY ./elasticsearch/src/ /app
COPY ./elasticsearch/requirements.txt /
RUN pip install -r requirements.txt
COPY ./database/schema.py /app/
WORKDIR /app
CMD python ingest_elastic.py
