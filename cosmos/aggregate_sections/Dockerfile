FROM python:3.7

# Install bash and vim
#RUN apk add bash vim

# Pillow requirements
#run apt install build-base jpeg-dev zlib-dev

COPY requirements.txt /

#For Debugging purpose
RUN apt-get update && apt-get -y install vim

RUN pip install -r /requirements.txt

COPY src/ /app
WORKDIR /app
