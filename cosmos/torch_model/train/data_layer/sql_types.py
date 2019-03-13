"""
Place holder for Sql Alchemy mappings
"""
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy import Column, Integer, String, LargeBinary, Sequence, PrimaryKeyConstraint, ForeignKeyConstraint, CheckConstraint
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
import pickle
Base = declarative_base()

class Neighbor(Base):
    """
    ORM mapping for Neighbors
    """
    __tablename__ = "neighborhoods"
    id = Column(Integer, Sequence("neighbor_id_sequence"), primary_key=True)
    center_object_id = Column(String)
    neighbor_object_id = Column(String)
    __table_args__ = (
        ForeignKeyConstraint(
            ["center_object_id"],
            ["examples.object_id"]),
        ForeignKeyConstraint(
            ["neighbor_object_id"],
            ["examples.object_id"]),
        CheckConstraint("center_object_id != neighbor_object_id", name="not_same_object_check")
            )
    neighbor = relationship("Example", 
            foreign_keys=[center_object_id], backref="neighbors")


class Example(Base):
    """
    ORM mapping for examples
    """
    __tablename__ = "examples"
    page_id = Column(String)
    object_id = Column(String)
    partition = Column(String)
    index = Column(Integer, autoincrement=True)
    _window = Column("window",LargeBinary)
    _bbox = Column("bbox", LargeBinary)
    _gt_box = Column("gt_box", LargeBinary)
    label = Column(String)
    __table_args__ = (PrimaryKeyConstraint("object_id",name="example_pk"),)

    @property
    def window(self):
        return pickle.loads(self._window)

    @window.setter
    def window(self, window):
        self._window = pickle.dumps(window)

    @property
    def bbox(self):
        return pickle.loads(self._bbox)

    @bbox.setter
    def bbox(self, bbox):
        self._bbox = pickle.dumps(bbox)

    @property
    def gt_box(self):
        return pickle.loads(self._gt_box)

    @gt_box.setter
    def gt_box(self, gt_box):
        self._gt_box = pickle.dumps(gt_box)

    def __repr__(self):
        return f"Example(id={self.object_id},{self.page_id},{self.object_id}, window={self.window.shape}, label=self.label, bbox={self.bbox})"


class ImageDB:
    """
    SQL alchemy session factory
    """
    @staticmethod
    def build(verbose=False):
        engine = create_engine('sqlite:///:memory:', echo=verbose)  
        Base.metadata.create_all(engine)
        Session = sessionmaker()
        Session.configure(bind=engine)
        return Session() 
