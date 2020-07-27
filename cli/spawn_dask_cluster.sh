#!/bin/bash
export OMP_NUM_THREADS=2
export MODEL_CONFIG=cosmos/ingestion/ingest/process/configs/model_config.yaml
export WEIGHTS_PTH=cosmos/weights/model_weights.pth
export DEVICE=cuda
declare -a bgpids

cleanup() {
    for pid in ${bgpids[@]}; do
        kill -9 $pid
    done
}
trap "cleanup" SIGINT SIGTERM

dask-scheduler &
bgpids+=("$!")
dask-worker tcp://localhost:8786 --nprocs $1 --nthreads 1 --memory-limit 0 --resources "process=1" &
bgpids+=("$!")
dask-worker tcp://localhost:8786 --nprocs $2 --nthreads 2 --memory-limit 0 --resources "GPU=1" --preload ingest.preload_plugins.detect_setup &
bgpids+=("$!")
wait