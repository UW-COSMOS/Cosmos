"""
Simple Sqlite DB for storing cosmos session information
"""
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from fastapi import HTTPException
from processing_session_types import Base, CosmosSessionJob

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
