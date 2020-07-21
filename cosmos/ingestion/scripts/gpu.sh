export MODEL_CONFIG=ingest/process/configs/model_config.yaml
export WEIGHTS_PTH=../weights/model_weights.pth
export DEVICE=cuda
export OMP_NUM_THREADS=2
dask-worker tcp://128.104.103.241:8786 --nprocs 1 --nthreads 2 --memory-limit 0 --resources "GPU=1" --preload ingest.detect_setup
