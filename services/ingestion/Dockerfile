FROM python:3.7-alpine

# Ghostscript
RUN apk add ghostscript=9.26-r2

# Pillow requirements
run apk add build-base jpeg-dev zlib-dev

COPY requirements.txt /

RUN pip install -r /requirements.txt

COPY src/ /app
WORKDIR /app

