FROM python:3.7

# Install bash and vim
#RUN apk add bash vim

# Pillow requirements
#run apt install build-base jpeg-dev zlib-dev

RUN pip install elasticsearch_dsl pymongo

COPY src/ /app
WORKDIR /app
