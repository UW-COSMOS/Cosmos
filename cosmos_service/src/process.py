"""
Helper script to run a single document through the COSMOS pipeline and then exit.
This is a workaround for running the pipeline within the main web-server process,
which leads to issues with locking the main thread.
"""
import os, sys
import tempfile
import make_parquet as mp
from fastapi.logger import logger
import uuid
from processing_session_types import CosmosSessionJob
from db import SessionLocal
import torch
from app import OOM_ERROR_EXIT_CODE

import shutil

os.environ["CUDA_VISIBLE_DEVICES"] = "0"
os.environ['MODEL_CONFIG']="/configs/model_config.yaml"
os.environ["WEIGHTS_PTH"]="/weights/model_weights.pth"
os.environ["PP_WEIGHTS_PTH"]="/weights/pp_model_weights.pth"
os.environ["AGGREGATIONS"]="pdfs,sections,tables,figures,equations"
os.environ["LD_LIBRARY_PATH"]="/usr/local/nvidia/lib:/usr/local/nvidia/lib64"

# list of observed messages related to insufficient GPU memory
# used to support retrying a job when there is not sufficient memory
# this list is non-exhaustive
OOM_ERROR_MESSAGES = [
    'CUDA out of memory',
    'CUDNN_STATUS_ALLOC_FAILED',
    'CUDNN_STATUS_NOT_INITIALIZED'
]

def process_document(pdf_dir: str, job_id: uuid.UUID):
    """
    Run a single document through the COSMOS pipeline.
    TODO: This adds the significant overhead of loading the model into memory with each run
    """
    with tempfile.TemporaryDirectory() as page_info_dir, tempfile.TemporaryDirectory() as cosmos_out_dir:
        with SessionLocal() as session:
            job = session.get(CosmosSessionJob, str(job_id))
            archive_out_dir = job.output_dir
            job.is_started = True
            
            session.commit()

        cosmos_error : Exception = None
        try: 
            mp.main_process(pdf_dir, page_info_dir, cosmos_out_dir)
            mp.resize_files(cosmos_out_dir)
            shutil.make_archive(f"{archive_out_dir}/cosmos_output", "zip", cosmos_out_dir)
        except Exception as e:
            cosmos_error = e
            print("Cosmos processing failed:\n", cosmos_error, flush=True)


        OOM_ERROR = cosmos_error and any([e in str(cosmos_error) for e in OOM_ERROR_MESSAGES])

        with SessionLocal() as session:
            job = session.get(CosmosSessionJob, str(job_id))
            job.is_completed = not OOM_ERROR # retry jobs that failed due to an OOM error
            job.error = None if OOM_ERROR or cosmos_error is None else str(cosmos_error)
            session.commit()

        if OOM_ERROR:
            exit(OOM_ERROR_EXIT_CODE)


if __name__ == '__main__':
    logger.info(torch.cuda.is_available())
    process_document(sys.argv[1], sys.argv[2])