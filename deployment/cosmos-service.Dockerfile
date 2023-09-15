ARG VERSION=latest
FROM uwcosmos/cosmos-ingestion:$VERSION

WORKDIR /src/
RUN apt install -y git
RUN pip3.8 install fastapi[all]
RUN pip3.8 install layoutparser
RUN pip3.8 install 'git+https://github.com/facebookresearch/detectron2.git@v0.4#egg=detectron2'
RUN pip3.8 uninstall -y Pillow
RUN pip3.8 install Pillow==9.0.0
RUN pip3.8 install ipython 
RUN pip3.8 install rocketry pydantic==1.10.10
COPY cosmos_service/src/ /src/
RUN apt install -y nano
ENV MODEL_CONFIG=/configs/model_config.yaml
ENV WEIGHTS_PTH=/weights/model_weights.pth
ENV PP_WEIGHTS_PTH=/weights/pp_model_weights.pth
COPY deployment/configs/lp_genseg_improvement_config.yaml /configs/
COPY deployment/weights/lp_genseg_improvement_model_final.pth /weights/
ENV AGGREGATIONS=pdfs,sections,tables,figures,equations
COPY htcosmos/make_parquet.py /src/
RUN mkdir /pdfs
RUN mkdir /pages
RUN mkdir /output
COPY 600de240ea8bd372268905df.pdf /pdfs/
COPY 616cf16267467f7269ccde6f.pdf /pdfs/

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8089"]
