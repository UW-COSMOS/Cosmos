"""
Place holder for Sql Alchemy mappings
"""
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy import Column, Integer, String, LargeBinary, Sequence, PrimaryKeyConstraint
from sqlalchemy.ext.declarative import declarative_base
import pickle
Base = declarative_base()

class Example(Base):
    """
    ORM mapping for examples
    """
    __tablename__ = "examples"
    doc_id = Column(Integer)
    page_id = Column(Integer)
    object_id = Column(Integer)
    _window = Column("window",LargeBinary)
    _bbox = Column("bbox", LargeBinary)
    label = Column(String)
    __table__args = (PrimaryKeyConstraint("doc_id", "page_id","object_id",name="example_pk"),)

    @property
    def window(self):
        return pickle.loads(self._window)

    @window.setter
    def window(self, window):
        self._window = pickle.dumps(window)


    def __repr__(self):
        return f"Example(id={self.id}, window={self.window.shape}, label=self.label)"


class Neighbor(Base):
    """
    ORM mapping for Neighbors
    """
    pass


class ImageDB:
    """
    SQL alchemy session factory
    """
    def __call__(self):
        engine = create_engine('sqlite:///:memory:', echo=True)  
        Base.metadata.create_all(engine)
        Session = sessionmaker()
        Session.configure(bind=engine)
        return Session() 
