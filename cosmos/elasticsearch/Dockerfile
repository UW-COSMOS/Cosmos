FROM python:3.7

# Install bash and vim
#RUN apk add bash vim

# Pillow requirements
#run apt install build-base jpeg-dev zlib-dev

RUN pip install elasticsearch_dsl pymongo click

COPY ./elasticsearch/src/ /app
COPY ./elasticsearch/requirements.txt /
RUN pip install -r requirements.txt
COPY ./database/schema.py /app/
WORKDIR /app
CMD python ingest_elastic.py
