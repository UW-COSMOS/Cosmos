"""
Place holder for Sql Alchemy mappings
"""
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy import Column, Integer, String, LargeBinary, Sequence, PrimaryKeyConstraint, ForeignKeyConstraint, CheckConstraint
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
    __table_args__ = (PrimaryKeyConstraint("doc_id", "page_id","object_id",name="example_pk"),)

    @property
    def window(self):
        return pickle.loads(self._window)

    @window.setter
    def window(self, window):
        self._window = pickle.dumps(window)

    @property
    def bbox(self):
        return pickle.loads(self._window)

    @bbox.setter
    def bbox(self, bbox):
        self._bbox = pickle.dumps(bbox)

    def __repr__(self):
        return f"Example(id={self.id}, window={self.window.shape}, label=self.label)"


class Neighbor(Base):
    """
    ORM mapping for Neighbors
    """
    __tablename__ = "neighborhoods"
    center_doc_id = Column(Integer)
    center_page_id = Column(Integer)
    center_object_id = Column(Integer)
    neighbor_doc_id = Column(Integer)
    neighbor_page_id = Column(Integer) 
    neighbor_object_id = Column(Integer)
    __table_args__ = (ForeignKeyConstraint(["center_doc_id", "center_page_id", "center_object_id"], ["examples.doc_id", "examples.page_id", "examples.object_id"]),
                        ForeignKeyConstraint(["neighbor_doc_id", "neighbor_page_id", "neighbor_object_id"], ["examples.doc_id", "examples.page_id", "examples.object_id"]),
                        CheckConstraint("center_page_id = neighbor_page_id", name="same_page_check"),
                        CheckConstraint("center_doc_id = neighbor_doc_id", name="same_doc_check"),
                        CheckConstraint("center_object_id != neighbor_object_id", name="not_same_object_check")
            )


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
