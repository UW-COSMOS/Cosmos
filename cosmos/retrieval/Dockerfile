FROM openjdk:11
RUN apt-get update && apt-get install -y \
  curl \
  wget \
  build-essential \
  maven \
  vim

RUN apt-get install -y gcc default-libmysqlclient-dev wget software-properties-common apt-transport-https

RUN wget \
    https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && bash Miniconda3-latest-Linux-x86_64.sh -b \
    && rm -f Miniconda3-latest-Linux-x86_64.sh
    
ENV PATH=/root/miniconda3/bin:$PATH
ENV CONDA_AUTO_UPDATE_CONDA=false

RUN /root/miniconda3/bin/conda create -y --name py37 python=3.7 \
   && /root/miniconda3/bin/conda clean -ya
ENV CONDA_DEFAULT_ENV=py37
ENV CONDA_PREFIX=/root/miniconda3/envs/$CONDA_DEFAULT_ENV
ENV PATH=$CONDA_PREFIX/bin:$PATH

COPY retrieval/requirements.txt /
RUN pip install -r /requirements.txt


RUN mkdir /app
COPY database/schema.py /app
COPY retrieval/retrieval /app
WORKDIR /app

CMD ["mvn clean package appassembler:assemble"]
