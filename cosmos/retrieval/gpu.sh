export RERANKING_MODEL_PATH=../weights/bert_reranker
export RERANKING_BASE_MODEL=bert-base-uncased
export RERANKING_BATCH_SIZE=16
export RERANKING_DEVICE=cuda
export RERANKING_NUM_WORKERS=1
export OMP_NUM_THREADS=8
dask-worker tcp://localhost:8786 --nprocs 1 --nthreads 8 --memory-limit 0 --resources "retrieval=1" --preload retrieval.bert_reranker.bert_reranking_retriever
