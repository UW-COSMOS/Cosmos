FROM python:3.7

COPY requirements.txt /

RUN pip install -r requirements.txt
RUN apt-get update && apt-get -y install vim

COPY src/ /app
WORKDIR /app/
