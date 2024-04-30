ARG VERSION=latest
FROM uwcosmos/cosmos-base:$VERSION

COPY deployment/weights/lp_mfd_improvement_model_final.pth /weights/lp_mfd_improvement_model_final.pth
COPY deployment/weights/model_weights.pth /weights/model_weights.pth
COPY deployment/weights/pp_model_weights.pth /weights/pp_model_weights.pth
COPY deployment/configs /configs
COPY cli/ingest_documents.sh /cli/ingest_documents.sh
RUN chmod +x /cli/ingest_documents.sh

# TODO this should probably go in cosmos-base, but there are some additional dependency issues to untangle there
RUN apt-get update && apt-get install -y git && apt-get install -y pkg-config
RUN pip3.8 uninstall -y pdfminer && \
    pip3.8 install fastapi[all] layoutparser 'git+https://github.com/facebookresearch/detectron2.git@v0.4#egg=detectron2' apscheduler && \
    pip3.8 install Pillow ipython pdfminer==20191125 pdfminer.six==20221105 pdfplumber==0.10.2 pymupdf

COPY cosmos/ingestion /ingestion
WORKDIR /ingestion
RUN python3.8 -m pip install .
ENV PYTHONPATH ".:${PYTHONPATH}"

CMD /cli/ingest_documents.sh
