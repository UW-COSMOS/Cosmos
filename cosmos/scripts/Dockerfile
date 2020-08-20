FROM python:3.7-alpine

RUN pip install requests tqdm
RUN mkdir /app
WORKDIR /app
COPY read_dir_and_request.py /app
CMD python read_dir_and_request.py /vol
