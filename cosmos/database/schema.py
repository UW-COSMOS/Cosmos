from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import requests
import logging
import os
import time


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
    bytes = Column(LargeBinary(length=(2**32)-1))
    content = Column(String(10000))
    bounding_box = Column(JSON)
    cls = Column(String(200))
    classification_success = Column(Boolean, unique=False, default=None)
    proposal_success = Column(Boolean, unique=False, default=None)


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

if __name__ == '__main__':
    main()



