FROM cosmoscpu:test

RUN apt-get update && apt-get install -y git libblas-dev liblapack-dev swig

RUN git clone https://github.com/facebookresearch/faiss.git
RUN cd faiss && cmake -DFAISS_ENABLE_GPU=OFF -DPython_EXECUTABLE=/usr/bin/python3.8 -B build . \
    && make -C build \
    && cd build/faiss/python && python3.8 setup.py install

RUN python3.8 -m pip install notebook jupyterlab ipywidgets
RUN jupyter nbextension enable --py widgetsnbextension
RUN apt-get install -y curl
RUN curl -sL https://deb.nodesource.com/setup_10.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install -y nodejs

RUN jupyter labextension install @jupyter-widgets/jupyterlab-manager

RUN python3.8 -m pip install spacy
RUN python3.8 -m spacy download en_core_web_sm
RUN python3.8 -m pip install ipdb

RUN wget https://s3-us-west-2.amazonaws.com/ai2-s2-scispacy/releases/v0.3.0/en_core_sci_lg-0.3.0.tar.gz
RUN python3.8 -m pip install ./en_core_sci_lg-0.3.0.tar.gz
RUN python3.8 -m pip install fuzzywuzzy
RUN python3.8 -m pip install scispacy


