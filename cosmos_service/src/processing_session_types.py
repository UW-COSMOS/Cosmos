"""
Place holder for Sql Alchemy mappings
"""
import random
from sqlalchemy import Column, Integer, String, TIMESTAMP
from sqlalchemy.orm import DeclarativeBase, relationship
from sqlalchemy.sql import func
from uuid import UUID

class Base(DeclarativeBase):
    pass


# TODO this is a rather simple model, might not need a sqlite DB for it

class CosmosSessionJob(Base):
    """
    ORM mapping for cosmos processing jobs
    """
    __tablename__ = "processing_jobs"
    id = Column(String, primary_key = True)
    created = Column(TIMESTAMP, default=func.now())
    started = Column(TIMESTAMP, default=None)
    completed = Column(TIMESTAMP, default=None)
    output_dir = Column(String)


    def __init__(self, id: UUID):
        self.id = str(id)


    @property
    def is_started(self):
        return self.started is not None

    @is_started.setter
    def is_started(self, value):
        if value:
            self.started = func.now()
        else:
            self.started = None

    @property
    def is_completed(self):
        return self.completed is not None

    @is_completed.setter
    def is_completed(self, value):
        if value:
            self.completed = func.now()
        else:
            self.completed = None
