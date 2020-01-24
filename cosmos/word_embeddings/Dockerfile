FROM python:3.7

COPY requirements.txt /
COPY download.py /
RUN pip install -r /requirements.txt
RUN python /download.py

COPY src/ /app
WORKDIR /app
