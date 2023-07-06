import os, sys
import tempfile
sys.path.append("..")
from fastapi import FastAPI, UploadFile, File, HTTPException, Depends
from fastapi.logger import logger
from fastapi.responses import FileResponse
import uuid
from processing_session_types import Base, CosmosSessionJob
from typing import List
import torch
import asyncio
from db import SessionLocal
from scheduler import scheduler

import shutil

app = FastAPI()

queue = asyncio.Queue()
workers : List[asyncio.Task] = None

# Approximate memory consumed by a single cosmos pipeline, used to calculate available
# concurrency

# TODO this only works assuming the cosmos api is the only process using the GPU, which
# is usually not the case
GPU_MEM_PER_WORKER = 4e9 # 4GB
COSMOS_SCRIPT = 'process.py'

# if a task fails due to the GPU being out of memory, wait for a while and then try again
OOM_SLEEP_TIME = 30
OOM_ERROR_EXIT_CODE = 2



async def cosmos_worker(work_queue: asyncio.Queue):
    """
    Cosmos worker process. Continually poll from the work queue for new parameters to the pipeline,
    and run the cosmos pipeline in a separate process. A separate process is necessary to avoid I/O 
    blocking issues in Python's async framework
    """
    while True:
        (job_output_dir, job_id) = await work_queue.get()
        proc = await asyncio.create_subprocess_exec(sys.executable, COSMOS_SCRIPT, job_output_dir, job_id)
        result = await proc.wait()
        queue.task_done()
        
        if result == OOM_ERROR_EXIT_CODE:
            await asyncio.sleep(OOM_SLEEP_TIME)
            await queue.put((job_output_dir, job_id))

@app.post("/process/", status_code=202)
async def process_document(pdf: UploadFile = File(...)):
    """
    Accept a new PDF document for COSMOS processing. Saves the PDF to disk, 
    then adds it to a queue for subsequent processing
    """
    job_id = str(uuid.uuid4())
    if not pdf.file or not pdf.filename:
        raise HTTPException(status_code=400, detail="Poorly constructed form upload")

    # Need to make a non-temporary directory to store PDF and job output due to
    # asynchronous processing and retrieval, must be cleaned up in separate job
    job_output_dir = f"{tempfile.gettempdir()}/{job_id}"
    os.mkdir(job_output_dir)
    try:
        with open(f"{job_output_dir}/{pdf.filename}", "wb") as f:
            shutil.copyfileobj(pdf.file, f)
    except Exception:
        return {"message": ":("}
    finally:
        pdf.file.close()

    # populate the job in its default state (not started)
    with SessionLocal() as session:
        session.add(CosmosSessionJob(job_id, pdf.filename.replace('.pdf', ''), job_output_dir))
        session.commit()

    await queue.put((job_output_dir, job_id))
    
    return {
        "message": "PDF Processing in Background", 
        "job_id": job_id, 
        "status_endpoint": f"/process/{job_id}/status",
        "result_endpoint": f"/process/{job_id}/result"
    }

@app.get("/process/{job_id}/status")
def get_processing_status(job_id: str):
    """
    Return the current status of a given pdf in the COSMOS processing queue. If `job.is_completed`,
    then the results of the processing can be retrieved from `/process/{job_id}/result`
    """
    with SessionLocal() as session:
        job : CosmosSessionJob = session.get(CosmosSessionJob, job_id)
        if not job:
            raise HTTPException(status_code=404, detail="Job not found")
        return { 
            "job_started": job.is_started,
            "job_completed": job.is_completed,
            "time_in_queue": job.time_in_queue,
            "time_processing": job.time_processing,
            "error": job.error
        }

@app.get("/process/{job_id}/result")
def get_processing_result(job_id: str):
    """
    Return the zip file containing the results of a completed COSMOS pipeline. Return status 400 if the 
    job is in a not-complete state
    """
    with SessionLocal() as session:
        job = session.get(CosmosSessionJob, job_id)
        if not job:
            raise HTTPException(status_code=404, detail="Job not found")
        elif not job.is_completed:
            raise HTTPException(status_code=400, detail="Job not finished")
    output_file = f"{job.pdf_name}_cosmos_output.zip"
    return FileResponse(f"{job.output_dir}/{output_file}", filename=output_file)


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
    global workers
    """
    Initialize FastAPI and add variables
    """
    max_worker_count = get_max_processes_per_gpu()
    logger.info(f"Creating {max_worker_count} work queues for COSMOS processing")
    workers = [asyncio.create_task(cosmos_worker(queue)) for _ in range(max_worker_count)]

    asyncio.create_task(scheduler.serve())

