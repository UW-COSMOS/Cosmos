FROM python:3.7
RUN pip install requests pytest pillow tqdm

RUN mkdir /app
WORKDIR /app
COPY scripts/read_dir_and_request.py /app
COPY tests/ /app

CMD pytest
