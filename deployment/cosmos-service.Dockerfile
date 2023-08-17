ARG VERSION=latest
FROM uwcosmos/cosmos-ingestion:$VERSION

WORKDIR /src/
RUN apt install -y git
RUN pip3.8 install fastapi[all]
RUN pip3.8 install layoutparser
RUN pip3.8 install 'git+https://github.com/facebookresearch/detectron2.git@v0.4#egg=detectron2'
RUN pip3.8 install rocketry pydantic==1.10.10
COPY cosmos_service/src/ /src/
COPY htcosmos/make_parquet.py /src/

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8089"]
