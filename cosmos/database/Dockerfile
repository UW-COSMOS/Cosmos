FROM python:3.7

RUN apt-get update
RUN apt-get install -y gcc default-libmysqlclient-dev 

RUN pip install mysqlclient sqlalchemy requests alembic click

RUN mkdir /app
WORKDIR /app
COPY schema.py /app
COPY alembic.ini /app
COPY migrations /app/migrations

