FROM nvidia/cuda:10.0-devel-ubuntu16.04

# Install some basic utilities
RUN apt-get update && apt-get install -y \
    curl \
    ca-certificates \
    sudo \
    git \
    bzip2 \
    libx11-6 \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir /app

WORKDIR /app

# Create a non-root user and switch to it
RUN adduser --disabled-password --gecos '' --shell /bin/bash user \
 && chown -R user:user /app
RUN echo "user ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/90-user
USER user

# All users can use /home/user as their home directory
ENV HOME=/home/user
RUN chmod 777 /home/user

# Install Miniconda
RUN curl -so ~/miniconda.sh https://repo.continuum.io/miniconda/Miniconda3-4.5.11-Linux-x86_64.sh \
 && chmod +x ~/miniconda.sh \
 && ~/miniconda.sh -b -p ~/miniconda \
 && rm ~/miniconda.sh
ENV PATH=/home/user/miniconda/bin:$PATH
ENV CONDA_AUTO_UPDATE_CONDA=false

# Create a Python 3.6 environment
RUN /home/user/miniconda/bin/conda install conda-build \
 && /home/user/miniconda/bin/conda create -y --name py36 python=3.7 \
 && /home/user/miniconda/bin/conda clean -ya
ENV CONDA_DEFAULT_ENV=py36
ENV CONDA_PREFIX=/home/user/miniconda/envs/$CONDA_DEFAULT_ENV
ENV PATH=$CONDA_PREFIX/bin:$PATH

# CUDA 10.0-specific steps
RUN conda install -y -c pytorch \
    cuda100=1.0 \
    magma-cuda100=2.4.0 \
    "pytorch=1.0.0=py3.7_cuda10.0.130_cudnn7.4.1_1" \
    torchvision=0.3.0 \
 && conda clean -ya

RUN pip install pymongo \
    pymongo     \
    pytest     \
    click     \
    cython     \
    tqdm     \
    numpy     \
    scipy     \
    pandas     \
    PyYAML     \
    matplotlib

RUN pip install numpy==1.15.4 \
        pyyaml \
        hyperyaml \
        scikit-image \
        bitmath \
        tensorboardX \ 
        tensorboard \
        tqdm \
        opencv-python \
        pascal-voc-writer \
        imgaug \
        Pillow \
        redis \
        sqlalchemy

RUN pip install joblib dataclasses

COPY weights/ /app
COPY src/ /app
WORKDIR /app
