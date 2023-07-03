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
import time
from sys import argv

import shutil

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

# creating an in-memory DB appears to create issues with session lifetime, use a flat file instead
engine = create_engine('sqlite:///sessions.db', echo=False)
Base.metadata.create_all(engine)
SessionLocal = sessionmaker(bind=engine)

def process_document_subprocess(pdf_dir: str, job_id: uuid.UUID):
    # We cannot delete the output directory until after the result has been retrieved. Use a directory in /tmp,
    # rather than a true temporary directory, to store it block is a crude way to accomplish this
    out_dir = f"{tempfile.gettempdir()}/{job_id}"
    os.mkdir(out_dir)
    with tempfile.TemporaryDirectory() as page_info_dir:
        with SessionLocal() as session:
            session.add(CosmosSessionJob(job_id, '', page_info_dir, out_dir))
            session.commit()

        results = mp.main_process(pdf_dir, page_info_dir, out_dir)
        mp.resize_files(out_dir)
        zip_file_name = f"{out_dir}/cosmos_output"
        shutil.make_archive(zip_file_name, "zip", out_dir)
        print(f"zip created at {zip_file_name}")
        print(f"{pdf_dir} for pdfs; {page_info_dir} for page info; {out_dir} for output")
        print(os.path.exists(zip_file_name + ".zip"))

        with SessionLocal() as session:
            job = session.get(CosmosSessionJob, str(job_id))
            job.completed = 1
            session.commit()


if __name__ == '__main__':
    import torch
    logger.info(torch.cuda.is_available())
    process_document_subprocess(sys.argv[1], sys.argv[2])
