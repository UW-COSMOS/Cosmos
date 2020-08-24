FROM nvidia/cuda:10.1-devel-ubuntu18.04

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

RUN conda update conda
RUN conda install -y \
    nomkl \
    pytorch torchvision cudatoolkit=10.1 numpy pandas dask scikit-learn sqlalchemy click beautifulsoup4 tqdm pyarrow tensorboard scikit-image xgboost opencv pdfminer.six tensorboardx gunicorn flask -c pytorch -c conda-forge && conda clean -afy
RUN pip install pascal-voc-writer pytesseract pikepdf hyperyaml transformers elasticsearch_dsl
