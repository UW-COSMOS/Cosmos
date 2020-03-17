from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import requests
import logging
import os
import time
from alembic.config import Config
from alembic import command


Base = declarative_base()

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class Pdf(Base):
    __tablename__ = 'pdfs'

    id = Column(Integer, primary_key=True)
    dataset_id = Column(String(200))
    pdf_name = Column(String(200), index=True)
    name = Column(String(200))
    meta = Column(JSON)
    meta_dimension = Column(JSON)
    pages = relationship('Page', back_populates='pdf')

    def __repr__(self):
        return f'<Pdf(id={self.id}, name={self.name}, metadata={self.metadata}, metadata_dimension={self.metadata_dimension}'

class Page(Base):
    __tablename__ = 'pages'

    id = Column(Integer, primary_key=True)
    pdf_id = Column(Integer, ForeignKey('pdfs.id'))
    pdf = relationship('Pdf', back_populates='pages')
    page_width = Column(Integer)
    page_height = Column(Integer)
    page_number = Column(Integer, index=True)
    bytes = Column(LargeBinary(length=(2**32)-1))

class PageObject(Base):
    __tablename__ = 'page_objects'

    id = Column(Integer, primary_key=True)
    page_id = Column(Integer, ForeignKey('pages.id'))
    context_id = Column(Integer, ForeignKey('object_contexts.id'))
    bytes = Column(LargeBinary(length=(2**32)-1))
    content = Column(String(10000))
    bounding_box = Column(JSON)
    cls = Column(String(200))
    confidence = Column(Numeric(precision=9, scale=6))
    classification_success = Column(Boolean, unique=False, default=None)
    proposal_success = Column(Boolean, unique=False, default=None)

class Section(Base):
    __tablename__ = 'sections'
    id = Column(Integer, primary_key=True)
    pdf_id = Column(Integer, ForeignKey('pdfs.id'))
    header = Column(String(200))
    content = Column(String(10000))
    objects = Column(String(200)) # oof, wish that mysql support arrays
    cls = Column(String(200))

class ObjectContext(Base):
    """
    An object context is a mapping between a page object and its associated contexts.
    Many PageObjects map to one ObjectContext
    Example: A Figure's ObjectContext contains the information in its Figure Caption
    """
    __tablename__ = 'object_contexts'
    id = Column(Integer, primary_key=True)
    pdf_id = Column(Integer, ForeignKey('pdfs.id'))
    cls = Column(String(200))
    header_id = Column(Integer, ForeignKey('page_objects.id'))
    header_content = Column(Text())
    content = Column(Text())


def ping_healthcheck():
    try:
        result = requests.get('http://dbwebapp:8081/health')
    except:
        return False
    return result.status_code == 200


def main():
    while(not ping_healthcheck()):
        logger.info('DB not up')
        time.sleep(1)

    logger.info('DB Up, creating schema')
    username = os.environ['DBUSER']
    password = os.environ['DBPASS']
    engine = create_engine(f'mysql://{username}:{password}@mysql-router:6446/cosmos')
    Base.metadata.create_all(engine)
    alembic_cfg = Config(os.environ['ALEMBIC_CFG'])
    command.stamp(alembic_cfg, "head")


if __name__ == '__main__':
    main()



