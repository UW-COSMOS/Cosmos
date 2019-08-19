FROM python:3.7

# Install bash and vim
#RUN apk add bash vim

# Pillow requirements
#run apt install build-base jpeg-dev zlib-dev

COPY requirements.txt /

RUN pip install -r /requirements.txt
RUN pip install elasticsearch_dsl

COPY src/ /app
WORKDIR /app
