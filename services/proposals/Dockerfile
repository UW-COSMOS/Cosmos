FROM python:3.7-alpine

# Install bash and vim
RUN apk add bash vim

# Pillow requirements
run apk add build-base jpeg-dev zlib-dev

COPY requirements.txt /

RUN pip install -r /requirements.txt

COPY src/ /app
WORKDIR /app
