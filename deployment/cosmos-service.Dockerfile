ARG VERSION=latest
FROM uwcosmos/cosmos-ingestion:$VERSION

ENV MODEL_CONFIG=/configs/model_config.yaml
ENV WEIGHTS_PTH=/weights/model_weights.pth
ENV PP_WEIGHTS_PTH=/weights/pp_model_weights.pth

WORKDIR /src/
RUN mkdir /pdfs /pages /output
RUN apt-get update && apt-get install -y git
RUN pip3.8 uninstall -y pdfplumber pdfminer && \
    pip3.8 install fastapi[all] layoutparser 'git+https://github.com/facebookresearch/detectron2.git@v0.4#egg=detectron2' apscheduler && \
    pip3.8 install Pillow ipython pdfminer==20191125 pdfminer.six==20221105 pdfplumber==0.10.2
COPY cosmos_service/src/ /src/
COPY deployment/configs/lp_genseg_improvement_config.yaml /configs/
COPY deployment/weights/lp_genseg_improvement_model_final.pth /weights/
ENV AGGREGATIONS=pdfs,sections,tables,figures,equations
COPY htcosmos/make_parquet.py /src/util/
CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8089"]
