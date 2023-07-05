"""
Place holder for Sql Alchemy mappings
"""
from sqlalchemy import Column, String, TIMESTAMP
from sqlalchemy.orm import DeclarativeBase, relationship
from sqlalchemy.sql import func
from uuid import UUID
from datetime import datetime

class Base(DeclarativeBase):
    pass


# TODO this is a rather simple model, might not need a sqlite DB for it

class CosmosSessionJob(Base):
    """
    ORM mapping for cosmos processing jobs
    """
    __tablename__ = "processing_jobs"
    id = Column(String, primary_key = True)
    created = Column(TIMESTAMP, default=None)
    started = Column(TIMESTAMP, default=None)
    completed = Column(TIMESTAMP, default=None)
    output_dir = Column(String)
    error = Column(String, default=None)


    def __init__(self, id: UUID, output_dir: str):
        self.id = str(id)
        self.output_dir = output_dir
        self.created = datetime.now()


    @property
    def is_started(self):
        return self.started is not None

    @is_started.setter
    def is_started(self, value):
        if value:
            self.started = datetime.now()
        else:
            self.started = None

    @property
    def is_completed(self):
        return self.completed is not None and self.error is None

    @is_completed.setter
    def is_completed(self, value):
        if value:
            self.completed = datetime.now()
        else:
            self.completed = None


    @property
    def time_in_queue(self):
        return (self.started - self.created if self.is_started else datetime.now() - self.created).total_seconds()

    @property
    def time_processing(self):
        if not self.is_started:
            return None
        return (self.completed - self.started if self.is_completed else datetime.now() - self.started).total_seconds()
