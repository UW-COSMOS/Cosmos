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
import click


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

    def __repr__(self):
        return f'<Page(id={self.id}, pdf_id={self.pdf_id}, page_width={self.page_width}, page_height={self.page_height}, page_number={self.page_number}>'

class PageObject(Base):
    __tablename__ = 'page_objects'

    id = Column(Integer, primary_key=True)
    page_id = Column(Integer, ForeignKey('pages.id'))
    context_id = Column(Integer, ForeignKey('object_contexts.id'))
    bytes = Column(LargeBinary(length=(2**32)-1))
    content = Column(String(10000))
    bounding_box = Column(JSON)
    init_cls_confidences = Column(JSON)
    cls = Column(String(200))
    pp_rule_cls = Column(String(200))
    annotated_cls = Column(String(200))
    confidence = Column(Numeric(precision=9, scale=6))
    classification_success = Column(Boolean, unique=False, default=None)
    proposal_success = Column(Boolean, unique=False, default=None)

class Table(Base):
    __tablename__ = 'tables'
    id = Column(Integer, primary_key=True)
    page_object_id = Column(Integer, ForeignKey('page_objects.id'))
    page_id = Column(Integer, ForeignKey('pages.id'))
    df = Column(LargeBinary(length=(2**32)-1))

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
    content = Column(Text(2**32-1))


def ping_healthcheck():
    try:
        result = requests.get('http://dbwebapp:8081/health')
    except:
        return False
    return result.status_code == 200


@click.command()
@click.option('--sqlite/--no-sqlite', default=False, help='Create an empty database using SQLite')
def main(sqlite):
    if sqlite:
        engine = create_engine('sqlite:////db/docs.db')
        Base.metadata.create_all(engine)
        return
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



