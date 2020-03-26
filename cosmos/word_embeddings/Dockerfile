FROM python:3.7

COPY ./word_embeddings/requirements.txt /
COPY ./word_embeddings/download.py /
RUN pip install -r /requirements.txt
RUN python /download.py

COPY ./word_embeddings/src/ /app
WORKDIR /app
