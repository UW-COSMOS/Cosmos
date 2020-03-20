import json
import shutil
import argparse
import os
from schema import ObjectContext, Pdf
from sqlalchemy.sql import text
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker 
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)


def convert(did, session, conn):
    dataset_pth = os.path.join('/index_dir', did)
    if os.path.exists(dataset_pth):
        logger.warning(f'Found the dataset directory already created {dataset_pth}. Deleting contents to repopulate')
        shutil.rmtree(dataset_pth)
    os.makedirs(dataset_pth)

    # Full contexts
    r = session.query(ObjectContext.id, ObjectContext.content).filter(ObjectContext.id, ObjectContext.pdf_id == Pdf.id).filter(Pdf.dataset_id == did)
    with open(os.path.join(dataset_pth, 'full_contexts.jsonl'), 'w') as wf, open(os.path.join(dataset_pth, 'raw.txt'), 'w') as rawf, open(os.path.join(dataset_pth, 'split_contexts.jsonl'), 'w') as splf:
        for id, content in r:
            example = {'id': str(id), 'contents': content}
            estr = json.dumps(example)
            wf.write(f'{estr}\n')
            rawf.write(f'{content}\n')
            # Sliding window, stride 64, window 256 (subword splitting downstream, assumption: avg 2 subwords / word)
            spl_content = content.split(' ')
            start = 0
            end = 256
            while end < len(spl_content):
                example = {'id': str(id), 'contents': ' '.join(spl_content[start:end])}
                estr = json.dumps(example)
                splf.write(f'{estr}\n')
                end += 64
                start += 64
            final_example = {'id': str(id), 'contents': ' '.join(spl_content[start:])}
            estr = json.dumps(final_example)
            splf.write(f'{estr}\n')

    # Just going to write some SQL here
    q = text('''SELECT pdfs.id, GROUP_CONCAT(object_contexts.content SEPARATOR '\n')
                FROM object_contexts, pdfs
                WHERE object_contexts.pdf_id = pdfs.id
                GROUP BY pdfs.id
                ''')
    r2 = conn.execute(q)
    with open(os.path.join(dataset_pth, 'documents.txt'), 'w') as wf:
        for pdf_id, content in r2:
            example = {'id': str(pdf_id), 'contents': content}
            estr = json.dumps(example)
            wf.write(f'{estr}\n')
    logger.info('done writing files')


if __name__ == '__main__':
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    did = os.environ["DATASET_ID"]
    conn = engine.connect()
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    convert(did, session, conn)

    

