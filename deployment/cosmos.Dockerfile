FROM nvidia/cuda:12.1.0-cudnn8-devel-ubuntu20.04

RUN apt-get update
ARG DEBIAN_FRONTEND="noninteractive"
ENV TZ=America/New_York
RUN apt-get install -y software-properties-common
RUN add-apt-repository ppa:deadsnakes/ppa

RUN apt-get update

RUN apt-get install -y --allow-unauthenticated tesseract-ocr
RUN apt-get install -y --fix-missing \
    ghostscript \
    gcc \
    wget \
    tesseract-ocr \
    apt-transport-https \
    libgl1-mesa-glx \
    build-essential \
    libpython3.8-dev \
    python3.8 \
    python3.8-distutils \
    python3-pip

RUN python3.8 -m pip install -U pip
RUN python3.8 -m pip install -v numpy

RUN DEBIAN_FRONTEND="noninteractive" TZ=America/New_York apt-get install -y python3-opencv

RUN rm -rf /var/lib/apt/lists/*

RUN python3.8 -m pip install torch torchvision torchaudio 

# Need this first for opencv
RUN python3.8 -m pip install scikit-build
RUN python3.8 -m pip install cmake

RUN python3.8 -m pip install --ignore-installed \
    pandas \
    dask['complete'] \
    click \
    beautifulsoup4 \
    tqdm \
    pyarrow \
    tensorboard \
    xgboost \
    pdfminer.six \
    tensorboardx \
    gunicorn \
    flask \
    pytesseract \
    transformers \
    elasticsearch_dsl \
    opencv-python \
    ftfy
