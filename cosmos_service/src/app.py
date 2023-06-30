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
from multiprocessing import Process
import time

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

def process_document_subprocess(pdf: UploadFile, job_id: uuid.UUID):
    # We cannot delete the output directory until after the result has been retrieved. Use a directory in /tmp,
    # rather than a true temporary directory, to store it block is a crude way to accomplish this
    out_dir = f"{tempfile.gettempdir()}/{job_id}"
    os.mkdir(out_dir)
    with tempfile.TemporaryDirectory() as pdf_dir, tempfile.TemporaryDirectory() as page_info_dir:
        try:
            with open(f"{pdf_dir}/{pdf.filename}", "wb") as f:
                shutil.copyfileobj(pdf.file, f)
        except Exception:
            return {"message": ":("}
        finally:
            pdf.file.close()

        with SessionLocal() as session:
            session.add(CosmosSessionJob(job_id, pdf.filename, page_info_dir, out_dir))
            session.commit()

        results = mp.main_process(pdf_dir, page_info_dir, out_dir)
        mp.resize_files(out_dir)
        zip_file_name = f"{out_dir}/{pdf.filename}_cosmos_output"
        shutil.make_archive(zip_file_name, "zip", out_dir)
        print(f"zip created at {zip_file_name}")
        print(f"{pdf_dir} for pdfs; {page_info_dir} for page info; {out_dir} for output")
        print(os.path.exists(zip_file_name + ".zip"))

        with SessionLocal() as session:
            job = session.get(CosmosSessionJob, str(job_id))
            job.completed = 1
            session.commit()

@app.post("/process/", status_code=202)
async def process_document(pdf: UploadFile = File(...), background_tasks: BackgroundTasks = None):
    job_id = uuid.uuid4()
    if not pdf.file or not pdf.filename:
        raise HTTPException(status_code=400, detail="Poorly constructed form upload")
    
    # use multiprocessing since parquet processing contains several blocking operations that cannot be easily
    # converted to a background task
    document_subprocess = Process(target=process_document_subprocess, args = (pdf, job_id))
    document_subprocess.start()
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
    """
    Initialize FastAPI and add variables
    """
    import torch
    torch.multiprocessing.set_start_method('spawn')
    logger.info(torch.cuda.is_available())

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
