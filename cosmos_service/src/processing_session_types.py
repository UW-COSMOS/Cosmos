"""
SQLAlchemy ORM mappings for the db entity(s) that track COSMOS document processing sessions
"""
from sqlalchemy import Column, String, TIMESTAMP
from sqlalchemy.orm import DeclarativeBase
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
    pdf_name = Column(String)
    output_dir = Column(String)
    pdf_hash = Column(String)
    pdf_length =Column(String)
    created = Column(TIMESTAMP, default=None)
    started = Column(TIMESTAMP, default=None)
    completed = Column(TIMESTAMP, default=None)
    error = Column(String, default=None)


    def __init__(self, id: UUID, pdf_name: str, pdf_hash: str, pdf_length: int, output_dir: str):
        self.id = str(id)
        self.pdf_name = pdf_name
        self.pdf_hash = pdf_hash,
        self.pdf_length = pdf_length
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
