FROM pytorch/pytorch:latest

#RUN pip install pytorch_transformers
COPY requirements.txt /

RUN pip install -r /requirements.txt
RUN apt-get update && apt-get -y install vim

COPY src/ /app
WORKDIR /app
