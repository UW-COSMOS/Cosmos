import os
from fastapi import FastAPI, APIRouter
from fastapi.middleware.gzip import GZipMiddleware
import torch
import asyncio
from scheduler import scheduler
from work_queue import setup_workers
from routers import process

prefix_url = os.environ.get('API_PREFIX','/cosmos_service')

app = FastAPI(title="COSMOS Service", docs_url=f"{prefix_url}/docs")

prefix_router = APIRouter(prefix=prefix_url)
prefix_router.include_router(process.router)
app.add_middleware(GZipMiddleware)

# Approximate memory consumed by a single cosmos pipeline, used to calculate available
# concurrency
# TODO this only works assuming the cosmos api is the only process using the GPU, which
# is usually not the case
GPU_MEM_PER_WORKER = 4e9 # 4GB


@prefix_router.get("/version_info")
def get_version_info():
    """Return the API version and git hash of the running API"""
    return {
        "version": os.environ.get("API_VERSION"),
        "git_hash": os.environ.get("GIT_HASH"),
    }


app.include_router(prefix_router)

def get_max_processes_per_gpu():
    """
    Approximately calculate the amount of cosmos pipelines that can be run in parallel based on
    the amount of memory available per GPU.
    TODO This assumes the COSMOS pipeline will be the only thing running on the GPU, which is
    not necessarily the case
    """
    if not torch.cuda.is_available():
        return 1
    max_mem = torch.cuda.get_device_properties(0).total_memory
    return int(max_mem / GPU_MEM_PER_WORKER)

@app.on_event("startup")
async def startup_event():
    """
    Initialize FastAPI and add variables
    """
    max_worker_count = get_max_processes_per_gpu()
    setup_workers(max_worker_count)

    asyncio.create_task(scheduler.serve())

