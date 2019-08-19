FROM python:3.7-alpine

# Install bash and vim
RUN apk add bash vim

# Ghostscript
RUN apk add ghostscript

# Pillow requirements
run apk add build-base jpeg-dev zlib-dev

COPY requirements.txt /

RUN pip install -r /requirements.txt


# Copy test documents to /test

#COPY tests/pdfs /test

COPY src/ /app

WORKDIR /app
