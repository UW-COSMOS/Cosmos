FROM pytorch/pytorch:1.3-cuda10.1-cudnn7-runtime

# Install some basic utilities
RUN apt-get update && apt-get install -y \
    curl \
    ca-certificates \
    sudo \
    git \
    bzip2 \
    libx11-6 

USER root

RUN apt-get -y install gcc g++ vim imagemagick tesseract-ocr libtesseract-dev
COPY ./requirements.txt .
RUN pip install -r requirements.txt --user

USER root


