FROM python:3.7

COPY requirements.txt /
COPY download.py /
RUN python /download.py

RUN pip install -r /requirements.txt
COPY src/ /app
WORKDIR /app
