FROM uwcosmos/cosmos-base:latest

# need this to get /lib64/libstdc++.so.6: version `CXXABI_1.3.9' working
# ImportError: /lib64/libstdc++.so.6: version `CXXABI_1.3.9' not found (required by /home/imcconnell2/miniconda3/envs/cosmos/lib/python3.8/site-packages/matplotlib/ft2font.cpython-38-x86_64-linux-gnu.so)
# RUN export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/imcconnell2/miniconda3/lib/
# RUN  apt-get install libstdc++6

WORKDIR /
COPY deployment/weights/model_weights.pth /weights/model_weights.pth
# COPY deployment/weights/pp_model_weights.pth /weights/pp_model_weights.pth
COPY deployment/configs /configs
COPY cli/train_postprocess_docker.sh /cli/train_postprocess_docker.sh
RUN chmod +x /cli/train_postprocess_docker.sh

RUN mkdir logs/
RUN mkdir train_dir/
RUN mkdir val_dir/
RUN mkdir out/

COPY cosmos/ingestion /ingestion
WORKDIR /ingestion
# RUN python3.8 -m pip install .
RUN python3.8 -m pip install -e .
ENV PYTHONPATH ".:${PYTHONPATH}"

WORKDIR /

CMD /cli/train_postprocess_docker.sh
