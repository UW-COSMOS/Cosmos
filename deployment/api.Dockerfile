FROM ankurgos/cosmos-base:latest

COPY cosmos/retrieval /retrieval
WORKDIR /retrieval
RUN pip install .

COPY cosmos/api /api
WORKDIR /api
RUN pip install .


