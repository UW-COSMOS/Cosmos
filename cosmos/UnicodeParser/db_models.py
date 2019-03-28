from fonduer.meta import Meta
from sqlalchemy.dialects import postgresql
from sqlalchemy import Column, Integer, String, Text, Float, ForeignKey

INT_ARRAY_TYPE = postgresql.ARRAY(Integer)
STR_ARRAY_TYPE = postgresql.ARRAY(String)

_meta = Meta.init()


class Equation(_meta.Base):
    """Equation class that represents an equation"""
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
    """Variable class that represents a variable object"""
    __tablename__ = "variable"
    id = Column(Integer, primary_key=True)
    text = Column(Text, nullable=False)
    document_id = Column(Integer)
    equation_id = Column(Integer, ForeignKey("equation.id"))
    equation_text = Column(Text, nullable=False)
    equation_offset = Column(Integer)
    sentence_id = Column(Integer, ForeignKey("sentence.id"))
    sentence_offset = Column(Integer)
    sentence_text = Column(Text, nullable=False)
    score = Column(Float)
    var_top = Column(Integer)
    var_bottom = Column(Integer)
    var_left = Column(Integer)
    var_right = Column(Integer)
    var_page = Column(Integer)

class TableX(_meta.Base):
    """Variables and Noun phrases extracted from sentences for each equation"""
    __tablename__ = "table_x"
    id = Column(Integer, primary_key=True)
    equation_id = Column(Integer, ForeignKey("equation.id"))
    symbols = Column(STR_ARRAY_TYPE)
    phrases = Column(STR_ARRAY_TYPE)
    phrases_top = Column(STR_ARRAY_TYPE)
    phrases_bottom = Column(STR_ARRAY_TYPE)
    phrases_left = Column(STR_ARRAY_TYPE)
    phrases_right = Column(STR_ARRAY_TYPE)
    phrases_page = Column(STR_ARRAY_TYPE)



