#Use pytorch as base image               
FROM pytorch/pytorch:1.3-cuda10.1-cudnn7-runtime
RUN apt-get update
RUN apt-get install -y gcc libmysqlclient-dev
COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt
RUN pip install pexpect --upgrade
RUN python -m spacy download en_core_web_lg

USER root

