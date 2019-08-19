FROM python:3.7-stretch

# Install bash and vim
#RUN apk add bash vim

# Pillow requirements
#run apt install build-base jpeg-dev zlib-dev
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-get install -y apt-transport-https
RUN echo 'deb https://notesalexp.org/tesseract-ocr/stretch/ stretch main' >> /etc/apt/sources.list
RUN apt-get update -oAcquire::AllowInsecureRepositories=true
RUN apt-get install -y --allow-unauthenticated notesalexp-keyring -oAcquire::AllowInsecureRepositories=true

RUN apt-get update

RUN apt-get install -y --allow-unauthenticated tesseract-ocr

COPY requirements.txt /

RUN pip install -r /requirements.txt

COPY src/ /app
WORKDIR /app
