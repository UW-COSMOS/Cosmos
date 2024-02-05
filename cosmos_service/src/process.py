"""
Helper script to run a single document through the COSMOS pipeline and then exit.
This is a workaround for running the pipeline within the main web-server process,
which leads to issues with locking the main thread.
"""
import os
import tempfile
import util.make_parquet as mp
from util.cosmos_output_utils import convert_parquet_to_json_file, PARQUET_SUFFIXES
from fastapi.logger import logger
from db.processing_session_types import CosmosSessionJob
from db.db import SessionLocal
import torch
import glob
from work_queue import OOM_ERROR_EXIT_CODE

import shutil
import argparse

os.environ["CUDA_VISIBLE_DEVICES"] = "0"
os.environ['MODEL_CONFIG']="/configs/lp_genseg_improvement_config.yaml"
#os.environ['MODEL_CONFIG']="/configs/model_config.yaml"
os.environ["WEIGHTS_PTH"]="/weights/lp_genseg_improvement_model_final.pth"
#os.environ["WEIGHTS_PTH"]="/weights/model_weights.pth"
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

def _get_parquet_files_to_convert(cosmos_out_dir, pdf_name):
    return [f'{cosmos_out_dir}/{pdf_name}{suffix}.parquet' for suffix in PARQUET_SUFFIXES]

def process_document(pdf_dir: str, job_id: str, compress_images: bool = True):
    """
    Run a single document through the COSMOS pipeline.
    TODO: This adds the significant overhead of loading the model into memory with each run
    """
    with tempfile.TemporaryDirectory() as page_info_dir, tempfile.TemporaryDirectory() as cosmos_out_dir:
        with SessionLocal() as session:
            job = session.get(CosmosSessionJob, job_id)
            archive_out_dir = f'{job.output_dir}/{job.pdf_name}_cosmos_output'
            pdf_name = job.pdf_name
            job.is_started = True
            
            session.commit()

        cosmos_error : Exception = None
        try: 
            mp.main_process(pdf_dir, page_info_dir, cosmos_out_dir)
            if compress_images:
                mp.resize_files(cosmos_out_dir)
            for parquet_path in _get_parquet_files_to_convert(cosmos_out_dir, pdf_name):
                convert_parquet_to_json_file(parquet_path)
            shutil.make_archive(archive_out_dir, "zip", cosmos_out_dir)
        except Exception as e:
            cosmos_error = e
            logger.exception("Cosmos processing failed")

        is_oom_error = cosmos_error and any([e in str(cosmos_error) for e in OOM_ERROR_MESSAGES])

        with SessionLocal() as session:
            job = session.get(CosmosSessionJob, job_id)
            job.is_completed = not is_oom_error # retry jobs that failed due to an OOM error
            job.error = None if is_oom_error or cosmos_error is None else str(cosmos_error)
            session.commit()

        if is_oom_error:
            exit(OOM_ERROR_EXIT_CODE)


if __name__ == '__main__':
    logger.info(torch.cuda.is_available())
    parser = argparse.ArgumentParser()
    parser.add_argument("pdf_dir")
    parser.add_argument("job_id")
    parser.add_argument("compress_images", type=lambda v: v.lower() == 'true', default=True)
    args = parser.parse_args()
    process_document(args.pdf_dir, args.job_id, args.compress_images)
