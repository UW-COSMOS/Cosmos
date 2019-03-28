import sys, os
sys.path.append(os.path.dirname(__file__))
from fonduer.meta import Meta
from sqlalchemy import Column, Integer, String, Text
from sqlalchemy.dialects import postgresql
import json
from fonduer.parser.models import Document, Sentence
from itertools import chain
from os.path import join
from db_models import Equation

INT_ARRAY_TYPE = postgresql.ARRAY(Integer)
STR_ARRAY_TYPE = postgresql.ARRAY(String)

# Grab pointer to global metadata
db_connect_str = "postgres://postgres:password@localhost:5432/cosmos8"


def insert_equation_tuple(db, resource_loc):
    """
    Insert equations information into the equation table by migrating tuples from the Sentence table.
    :param db: db connection string.
    :param resource_loc: Directory storing the json files which contain the equation coordinate information. 
    """
    session = Meta.init(db).Session()
    for doc in session.query(Document):
        locs = json.load(open(join(resource_loc, '%s.html.json' % doc.name)))
        print(join(resource_loc, '%s.html.json' % doc.name))
        locs_counter = 0
        for sent in session.query(Sentence).filter(Sentence.document_id == doc.id).order_by(Sentence.paragraph_id):
            if sent.name == 'Equation':
                length_tmp = len(locs[locs_counter]['text'])
                if not sent.text.replace('-', '—').replace('−','—')\
                       .startswith(locs[locs_counter]['text'][:min(5,length_tmp-1)].replace('-', '—').replace('−','—')):
                    print('Not Aligned!!!')
                    #print(sent.id)
                    #print('*****************************************')
                    #print(sent.text)
                    #print('-----------------------------------------')
                    #print(locs[locs_counter]['text'])
                    #print('*****************************************')
                e = Equation(
                    id = sent.id, name='Equation', document_id=doc.id, section_id=sent.section_id, paragraph_id=sent.paragraph_id,
                    text=sent.text, variables=[],
                    top=locs[locs_counter]['ymin'],
                    bottom=locs[locs_counter]['ymax'],
                    left=locs[locs_counter]['xmin'],
                    right=locs[locs_counter]['xmax'],
                    page=locs[locs_counter]['page_num']
                )

                session.add(e)
                locs_counter += 1
        session.commit()


if __name__ == '__main__':
    insert_equation_tuple(db_connect_str, 'out/equations/')
    # session = Meta.init(db_connect_str).Session()
    # session.add(Equation(name="Equation", latex="\\tfrac{a}{b}"))
    # session.commit()
