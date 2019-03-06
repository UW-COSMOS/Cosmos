import os
os.environ["CORENLP_HOME"] = "/home/vangle/corenlp/stanford-corenlp-full-2018-10-05"

import re
import string
from stanfordnlp.server import CoreNLPClient
from db_models import Equation, Variable, TableX
from sqlalchemy.dialects import postgresql
from fonduer.meta import Meta

from stanfordnlp.server import CoreNLPClient

db_connect_str = "postgres://postgres:vangle@localhost:5432/cosmos5"

def build_table_X(db):
    with open('words_alpha.txt') as word_file:
        valid_words = set(word_file.read().split())
    session = Meta.init(db).Session()
    variables = session.query(Variable).order_by(Variable.equation_id)
    equations = session.query(Equation).order_by(Equation.id)
    with CoreNLPClient(annotators=['openie']) as client:
        for eqt in equations:
            print(eqt.id)
            vars_in_eqt = variables.filter(Variable.equation_id == eqt.id)
            if vars_in_eqt.count() == 0:
                print('No Variable found for equation '+str(eqt.id))
            else:
                vars_used = []
                sent_used = []
                entities = []

                for var in vars_in_eqt:
                    text = var.text.strip(',').strip('.').strip('?')
                    if text not in vars_used:
                        vars_used.append(text)
                for var in vars_in_eqt:
                    sent_id = var.sentence_id
                    if sent_id not in sent_used:
                        sent_used.append(sent_id)
                        sent_text = var.sentence_text
                        ann = client.annotate(sent_text)
                        for sent in ann.sentence:
                            for triple in sent.openieTriple:
                                sub = triple.subject
                                obj = triple.object
                                tokens = re.sub('['+string.punctuation+']', ' ', sub)
                                tokens = tokens.split()
                                good_entity = True
                                for token in tokens:
                                    if token not in valid_words:
                                        good_entity = False

                                if good_entity and sub not in vars_used and sub not in entities:
                                    entities.append(sub)
                                
                                tokens = re.sub('['+string.punctuation+']', ' ', obj)
                                tokens = tokens.split()
                                good_entity = True
                                for token in tokens:
                                    if token not in valid_words:
                                        good_entity = False

                                if good_entity and obj not in vars_used and obj not in entities:
                                    entities.append(obj)

                x = TableX(
                    equation_id = eqt.id,
                    symbols = vars_used,
                    phrases = entities
                )
                
                session.add(x)

        session.commit()






if __name__ == '__main__':
    build_table_X(db_connect_str)


