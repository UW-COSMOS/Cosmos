FROM pytorch/pytorch:1.3-cuda10.1-cudnn7-runtime

RUN apt-get update
RUN apt-get install -y gcc libmysqlclient-dev 

RUN pip install git+https://github.com/williamFalcon/pytorch-lightning.git@master --upgrade
COPY semantic_segmentation/requirements.txt /
RUN pip install -r /requirements.txt
COPY semantic_segmentation/src/ /app
COPY database/schema.py /app
WORKDIR /app

