FROM nvidia/cuda:10.0-devel-ubuntu18.04

ENV PATH="/root/miniconda3/bin:${PATH}"
ARG PATH="/root/miniconda3/bin:${PATH}"

RUN apt-get update

RUN apt-get install -y --allow-unauthenticated tesseract-ocr
RUN apt-get install -y ghostscript gcc libmysqlclient-dev wget tesseract-ocr software-properties-common apt-transport-https

RUN rm -rf /var/lib/apt/lists/*
RUN wget \
    https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && mkdir /root/.conda \
    && bash Miniconda3-latest-Linux-x86_64.sh -b \
    && rm -f Miniconda3-latest-Linux-x86_64.sh 

COPY process_pages/requirements.txt /
RUN pip install -r /requirements.txt


# Copy test documents to /test

#COPY tests/pdfs /test

COPY process_pages/process /app
COPY database/schema.py /app/schema

WORKDIR /
