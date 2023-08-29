"""
Simple Sqlite DB for storing cosmos session information
"""
from typing import BinaryIO
from sqlalchemy import create_engine, select
from sqlalchemy.orm import sessionmaker
from fastapi import HTTPException
from processing_session_types import Base, CosmosSessionJob
from util.hash_file import hash_file
import io
import hashlib


engine = create_engine('sqlite:///sessions.db', echo=False)
Base.metadata.create_all(engine)
SessionLocal = sessionmaker(bind=engine)

def get_job_details(job_id: str) -> CosmosSessionJob:
    with SessionLocal() as session:
        job = session.get(CosmosSessionJob, job_id)
        if not job:
            raise HTTPException(status_code=404, detail="Job not found")
        elif not job.is_completed:
            raise HTTPException(status_code=400, detail="Job not finished")
        return job


def get_cached_job_for_pdf(pdf_data: BinaryIO) -> CosmosSessionJob:
    pdf_data.seek(0, io.SEEK_END)
    pdf_length = pdf_data.tell()

    pdf_data.seek(0, io.SEEK_SET)
    pdf_sha1 = hash_file(pdf_data)
    pdf_data.seek(0, io.SEEK_SET)

    with SessionLocal() as session:
        result = session.execute(select(CosmosSessionJob).where(
            CosmosSessionJob.pdf_hash == pdf_sha1 and CosmosSessionJob.pdf_length == pdf_length
        )).first()
        return pdf_sha1, pdf_length, result
