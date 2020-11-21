FROM uwcosmos/cosmos-base:latest

WORKDIR /
COPY deployment/weights/model_weights.pth /weights/model_weights.pth
COPY deployment/configs /configs
COPY cli/train_postprocess_docker.sh /cli/train_postprocess_docker.sh
RUN chmod +x /cli/train_postprocess_docker.sh

RUN mkdir logs/
RUN mkdir train_dir/
RUN mkdir val_dir/
RUN mkdir out/

COPY cosmos/ingestion /ingestion
WORKDIR /ingestion
RUN python3.8 -m pip install -e .
ENV PYTHONPATH ".:${PYTHONPATH}"

WORKDIR /

CMD /cli/train_postprocess_docker.sh
