FROM uwcosmos/cosmos-base:latest

COPY cosmos/retrieval /retrieval
WORKDIR /retrieval
RUN python3.8 -m pip install .

COPY cosmos/api /api
WORKDIR /api
RUN python3.8 -m pip install .
RUN python3.8 -m pip install flask_cors
