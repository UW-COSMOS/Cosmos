FROM python:3.7


RUN apt-get update
RUN apt-get install -y gcc default-libmysqlclient-dev

COPY page_api/requirements.txt /

RUN pip install -r /requirements.txt

COPY page_api/src/ /app
COPY database/schema.py /app
WORKDIR /app
