FROM python:3.7

# Install bash and vim
#RUN apk add bash vim

# Pillow requirements
#run apt install build-base jpeg-dev zlib-dev

COPY ./aggregate_sections/requirements.txt /

#For Debugging purpose
RUN apt-get update && apt-get -y install vim

RUN pip install -r /requirements.txt

COPY ./aggregate_sections/src/ /app
COPY ./database/schema.py /app
WORKDIR /app
