import os, sys
import tempfile
sys.path.append("..")
import make_parquet as mp
from pydantic import BaseModel
from fastapi import FastAPI, UploadFile, File, BackgroundTasks, HTTPException, Depends
from fastapi.logger import logger
from fastapi.responses import FileResponse
import uuid
from sqlalchemy import create_engine, update, select
from sqlalchemy.orm import sessionmaker
from processing_session_types import Base, CosmosSessionJob
from subprocess import Popen
from typing import List
from datetime import datetime
import torch
import asyncio

import shutil

# creating an in-memory DB appears to create issues with session lifetime, use a flat file instead
engine = create_engine('sqlite:///sessions.db', echo=False)
Base.metadata.create_all(engine)
SessionLocal = sessionmaker(bind=engine)

app = FastAPI()

#os.environ["KEEP_INFO"] = "True"
#os.environ["JUST_PROPOSE"] = "True"
#os.environ["SKIP_AGGREGATION"]  = "True"
#os.environ["JUST_AGGREGATION"] = "True"
os.environ["CUDA_VISIBLE_DEVICES"] = "0"
os.environ['MODEL_CONFIG']="/configs/model_config.yaml"
os.environ["WEIGHTS_PTH"]="/weights/model_weights.pth"
os.environ["PP_WEIGHTS_PTH"]="/weights/pp_model_weights.pth"
os.environ["AGGREGATIONS"]="pdfs,sections,tables,figures,equations"
os.environ["LD_LIBRARY_PATH"]="/usr/local/nvidia/lib:/usr/local/nvidia/lib64"

queue = asyncio.Queue()
workers : List[asyncio.Task] = None

# Approximate memory consumed by a single cosmos pipeline, used to calculate available
# concurrency
GPU_MEM_PER_WORKER = 4e9 # 4GB
COSMOS_SCRIPT = 'process.py'

async def cosmos_worker(work_queue: asyncio.Queue):
    """
    Cosmos worker process. Continually poll from the work queue for new parameters to the pipeline,
    and run the cosmos pipeline in a separate process. A separate process is necessary to avoid I/O 
    blocking issues in Python's async framework
    """
    while True:
        (pdf_dir, job_id) = await work_queue.get()
        proc = await asyncio.create_subprocess_exec('python3.8', COSMOS_SCRIPT, pdf_dir, job_id)
        await proc.wait()
        queue.task_done()


@app.post("/process/", status_code=202)
async def process_document(pdf: UploadFile = File(...)):
    job_id = str(uuid.uuid4())
    if not pdf.file or not pdf.filename:
        raise HTTPException(status_code=400, detail="Poorly constructed form upload")

    pdf_dir = f"{tempfile.gettempdir()}/{job_id}-pdf"
    os.mkdir(pdf_dir)
    try:
        with open(f"{pdf_dir}/{pdf.filename}", "wb") as f:
            shutil.copyfileobj(pdf.file, f)
    except Exception:
        return {"message": ":("}
    finally:
        pdf.file.close()

    # populate the job in its default state (not started)
    with SessionLocal() as session:
        session.add(CosmosSessionJob(job_id))
        session.commit()

    await queue.put((pdf_dir, job_id))
    
    return {
        "message": "PDF Processing in Background", 
        "job_id": job_id, 
        "status_endpoint": f"/process/{job_id}/status",
        "result_endpoint": f"/process/{job_id}/result",
    }

@app.get("/process/{job_id}/status")
def get_processing_status(job_id: str):
    with SessionLocal() as session:
        job = session.get(CosmosSessionJob, job_id)
        if not job:
            raise HTTPException(status_code=404, detail="Job not found")
        return { 
            "job_started": job.is_started,
            "job_completed": job.is_completed,
            "time_in_queue": job.time_in_queue,
            "time_processing": job.time_processing
        }

@app.get("/process/{job_id}/result")
def get_processing_result(job_id: str):
    with SessionLocal() as session:
        job = session.get(CosmosSessionJob, job_id)
        if not job:
            raise HTTPException(status_code=404, detail="Job not found")
        elif not job.is_completed:
            raise HTTPException(status_code=400, detail="Job not finished")
    output_file = f"{job.output_dir}/cosmos_output.zip"
    return FileResponse(output_file)


def get_max_processes_per_gpu():
    """
    (Crudely) calculate the amount of cosmos pipelines that can be run in parallel based on
    the amount of memory available per CPU, and a
    """
    if not torch.cuda.is_available:
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
    logger.info(f"{torch.cuda.is_available()}, {max_worker_count}")
    workers = [asyncio.create_task(cosmos_worker(queue)) for _ in range(max_worker_count)]

#    # Initialize the pytorch model
#    model = Model()
#    model.load_state_dict(torch.load(
#        CONFIG['MODEL_PATH'], map_location=torch.device(CONFIG['DEVICE'])))
#    model.eval()
#
#    # add model and other preprocess tools too app state
#    app.package = {
#        "scaler": load(CONFIG['SCALAR_PATH']),  # joblib.load
#        "model": model
#    }
