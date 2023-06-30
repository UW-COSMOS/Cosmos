"""
Place holder for Sql Alchemy mappings
"""
import random
from sqlalchemy import Column, Integer, String, TIMESTAMP
from sqlalchemy.orm import DeclarativeBase, relationship
from sqlalchemy.sql import func
from uuid import UUID
import pickle

class Base(DeclarativeBase):
    pass


# TODO this is a rather simple model, might not need a sqlite DB for it

class CosmosSessionJob(Base):
    """
    ORM mapping for cosmos processing jobs
    """
    __tablename__ = "processing_jobs"
    id = Column(String, primary_key = True)
    filename = Column(String)
    started = Column(TIMESTAMP, default = func.now())
    work_dir = Column(String)
    output_dir = Column(String)
    completed = Column(Integer, default = 0)
    total_pages = Column(Integer, default = 0)
    processed_pages = Column(Integer, default = 0)


    def __init__(self, id: UUID, filename: str, work_dir: str, output_dir: str):
        self.id = str(id)
        self.filename = filename
        self.work_dir = work_dir
        self.output_dir = output_dir

