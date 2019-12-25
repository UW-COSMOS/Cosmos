from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import click

Base = declarative_base()

class Pdf(Base):
    __tablename__ = 'pdfs'

    id = Column(Integer, primary_key=True)
    dataset_id = Column(String(200))
    name = Column(String(200))
    meta = Column(JSON)
    meta_dimension = Column(JSON)
    bytes = Column(LargeBinary(length=(2**32)-1))
    pages = relationship('Page', back_populates='pdf')

    def __repr__(self):
        return f'<Pdf(id={self.id}, name={self.name}, metadata={self.metadata}, metadata_dimension={self.metadata_dimension}'

class Page(Base):
    __tablename__ = 'pages'

    id = Column(Integer, primary_key=True)
    pdf_id = Column(Integer, ForeignKey('pdfs.id'))
    pdf = relationship('Pdf', back_populates='pages')
    bytes = Column(LargeBinary(length=(2**32)-1))
    page_width = Column(Integer)
    page_height = Column(Integer)
    page_number = Column(Integer)


@click.command()
@click.argument('username')
@click.argument('password')
def main(username, password):
    engine = create_engine(f'mysql://{username}:{password}@database/cosmos')
    Base.metadata.create_all(engine)

if __name__ == '__main__':
    main()
    


