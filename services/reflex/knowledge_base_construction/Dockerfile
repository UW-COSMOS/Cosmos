#FROM nvidia/cuda:10.0-devel-ubuntu16.04
#
## Install some basic utilities
#RUN apt-get update && apt-get install -y \
#    curl \
#    ca-certificates \
#    sudo \
#    git \
#    bzip2 \
#    libx11-6 \
#    wget \
#    build-essential \
#    libssl-dev \
#    libffi-dev \
# && rm -rf /var/lib/apt/lists/*
#
#
## Create a non-root user and switch to it
#RUN adduser --disabled-password --gecos '' --shell /bin/bash user \
# && chown -R user:user /app
#RUN echo "user ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/90-user
#USER user
#
## All users can use /home/user as their home directory
#ENV HOME=/home/user
#RUN chmod 777 /home/user
#COPY /src /kbc
#WORKDIR /kbc
## Install Miniconda
#RUN curl -so ~/miniconda.sh https://repo.continuum.io/miniconda/Miniconda3-4.5.11-Linux-x86_64.sh \
# && chmod +x ~/miniconda.sh \
# && ~/miniconda.sh -b -p ~/miniconda \
# && rm ~/miniconda.sh
#ENV PATH=/home/user/miniconda/bin:$PATH
#ENV CONDA_AUTO_UPDATE_CONDA=false
#
## Create a Python 3.6 environment
#RUN /home/user/miniconda/bin/conda install conda-build \
# && /home/user/miniconda/bin/conda create -y --name py36 python=3.7 \
# && /home/user/miniconda/bin/conda clean -ya
#ENV CONDA_DEFAULT_ENV=py36
#ENV CONDA_PREFIX=/home/user/miniconda/envs/$CONDA_DEFAULT_ENV
#ENV PATH=$CONDA_PREFIX/bin:$PATH
#
## CUDA 10.0-specific steps
#RUN conda install -y -c pytorch \
#    torchvision \
#    cudatoolkit=10.0 \
#    pytorch \
# && conda clean -ya
#
                
FROM pytorch/pytorch:1.3-cuda10.1-cudnn7-runtime
RUN apt-get update
RUN apt-get install -y gcc libmysqlclient-dev
COPY requirements.txt requirements.txt
#COPY install.sh install.sh
RUN pip install -r requirements.txt
RUN pip install pexpect --upgrade
RUN python -m spacy download en_core_web_lg

#RUN bash install.sh
USER root

