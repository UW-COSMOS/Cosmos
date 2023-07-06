"""
Simple Sqlite DB for storing cosmos session information
"""
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from processing_session_types import Base

engine = create_engine('sqlite:///sessions.db', echo=False)
Base.metadata.create_all(engine)
SessionLocal = sessionmaker(bind=engine)
