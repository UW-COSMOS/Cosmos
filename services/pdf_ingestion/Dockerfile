FROM python:3.7-alpine

RUN apk add ghostscript=9.26-r2

COPY requirements.txt /

RUN pip install -r /requirements.txt

COPY src/ /app
WORKDIR /app

