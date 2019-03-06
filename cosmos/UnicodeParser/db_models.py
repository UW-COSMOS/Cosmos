from fonduer.meta import Meta
from sqlalchemy.dialects import postgresql
from sqlalchemy import Column, Integer, String, Text, Float

INT_ARRAY_TYPE = postgresql.ARRAY(Integer)
STR_ARRAY_TYPE = postgresql.ARRAY(String)

_meta = Meta.init()


class Equation(_meta.Base):
    __tablename__ = "equation"
    id = Column(Integer, primary_key=True)
    
    name = Column(String, unique=False, nullable=True)
    document_id = Column(Integer)
    section_id = Column(Integer)
    paragraph_id = Column(Integer)
    text = Column(Text, nullable=False)
    variables = Column(STR_ARRAY_TYPE)
    top = Column(Integer)
    bottom = Column(Integer)
    left = Column(Integer)
    right = Column(Integer)
    page = Column(Integer)

class Variable(_meta.Base):
    __tablename__ = "variable"
    id = Column(Integer, primary_key=True)
    text = Column(Text, nullable=False)
    document_id = Column(Integer)
    equation_id = Column(Integer)
    equation_text = Column(Text, nullable=False)
    equation_offset = Column(Integer)
    sentence_id = Column(Integer)
    sentence_offset = Column(Integer)
    sentence_text = Column(Text, nullable=False)
    score = Column(Float)

class TableX(_meta.Base):
    __tablename__ = "table_x"
    equation_id = Column(Integer, primary_key=True)
    symbols = Column(STR_ARRAY_TYPE)
    phrases = Column(STR_ARRAY_TYPE)

