FROM python:3.7


RUN apt-get update

COPY ./requirements.txt /

RUN pip install -r /requirements.txt

COPY ./birdnest_backend /app
WORKDIR /app