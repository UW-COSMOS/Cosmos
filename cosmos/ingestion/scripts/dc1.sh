export OMP_NUM_THREADS=1
dask-worker tcp://128.104.103.241:8786 --nprocs $1 --nthreads 1 --memory-limit 0 --resources "process=1"
