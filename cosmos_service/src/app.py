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
import time
from process import process_document_subprocess
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

# number of cosmos pipeline work queues to run in parallel
WORKER_COUNT = 1

async def cosmos_worker(work_queue: asyncio.Queue):
    """
    Cosmos worker process. Continually poll from the work queue for new parameters to the pipeline,
    and run the process
    """
    while True:
        (pdf_dir, job_id) = await work_queue.get()
        try:
            process_document_subprocess(pdf_dir, job_id)
        except Exception as e:
            print("Something went wrong!", flush=True)
            print(e, flush=True)
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
        return { "job_complete": job.completed }

@app.get("/process/{job_id}/result")
def get_processing_result(job_id: str):
    with SessionLocal() as session:
        job = session.get(CosmosSessionJob, job_id)
        if not job:
            raise HTTPException(status_code=404, detail="Job not found")
        elif not job.completed:
            raise HTTPException(status_code=400, detail="Job not finished")
    output_file = f"{job.output_dir}/{job.filename}_cosmos_output.zip"
    return FileResponse(output_file)


@app.get("/")
def read_root():
    return {"Hello": "World"}

@app.on_event("startup")
async def startup_event():
    global workers
    """
    Initialize FastAPI and add variables
    """
    import torch
    logger.info(torch.cuda.is_available())
    workers = [asyncio.create_task(cosmos_worker(queue)) for _ in range(WORKER_COUNT)]

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
