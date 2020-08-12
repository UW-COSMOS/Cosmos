#!/bin/bash
export OMP_NUM_THREADS=2
export MODEL_CONFIG=cosmos/config/model_config.yaml
export WEIGHTS_PTH=cosmos/weights/amfam_weights.pth
export PP_WEIGHTS_PTH=cosmos/weights/pp_amfam_weights.pth
export CLASSES_PTH=cosmos/config/model_config.yaml
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
CUDA_VISIBLE_DEVICES=0 dask-worker tcp://localhost:8786 --nprocs $2 --nthreads 2 --memory-limit 0 --resources "GPU=1" --preload ingest.preload_plugins.detect_setup &
bgpids+=("$!")
CUDA_VISIBLE_DEVICES=1 dask-worker tcp://localhost:8786 --nprocs $2 --nthreads 2 --memory-limit 0 --resources "GPU=1" --preload ingest.preload_plugins.detect_setup &
bgpids+=("$!")
dask-worker tcp://localhost:8786 --nprocs $1 --nthreads 1 --memory-limit 0 --resources "process=1" --preload ingest.preload_plugins.process_setup &
bgpids+=("$!")
wait