import os
import re
import string
from stanfordnlp.server import CoreNLPClient
from db_models import Equation, Variable, TableX
from sqlalchemy.dialects import postgresql
from fonduer.meta import Meta

from stanfordnlp.server import CoreNLPClient

db_connect_str = "postgres://postgres:password@localhost:5432/cosmos_unicode_1"


def build_table_X(db, corenlp):
    os.environ["CORENLP_HOME"] = corenlp

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
                print('No Variable found for equation ' + str(eqt.id))
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
                                tokens = re.sub('[' + string.punctuation + ']', ' ', sub)
                                tokens = tokens.split()
                                good_entity = True
                                valid_phrase = ''
                                for token in tokens:
                                    for char in token:
                                        if not char.isalpha():
                                            #print('Bad entity: '+sub)
                                            good_entity = False
                                    if token not in vars_used:
                                        valid_phrase += token+' '

                                if good_entity and len(valid_phrase) > 0 and valid_phrase not in vars_used and valid_phrase not in entities:
                                    entities.append(valid_phrase)

                                tokens = re.sub('[' + string.punctuation + ']', ' ', obj)
                                tokens = tokens.split()
                                good_entity = True
                                valid_phrase = ''
                                for token in tokens:
                                    for char in token:
                                        if not char.isalpha():
                                            #print('Bad entity: '+obj)
                                            good_entity = False
                                    if token not in vars_used:
                                        valid_phrase += token+' '

                                if good_entity and len(valid_phrase) > 0 and valid_phrase not in vars_used and valid_phrase not in entities:
                                    entities.append(valid_phrase)

                x = TableX(
                    equation_id=eqt.id,
                    symbols=vars_used,
                    phrases=entities
                )

                session.add(x)

        session.commit()


if __name__ == '__main__':
    build_table_X(db_connect_str)
