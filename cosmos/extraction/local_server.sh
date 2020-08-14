export QA_MODEL_PATH=/ssd/ankur/Cosmos/deployment/weights/bert_base_squad2
export QA_DEVICE=cuda
export OMP_NUM_THREADS=8
dask-worker tcp://localhost:8786 --nprocs 1 --nthreads 8 --memory-limit 0 --resources "qa=1" --preload extraction.qa_extractor
