import json
import shutil
import argparse
import os
import subprocess
from schema import ObjectContext, Pdf
from sqlalchemy.sql import text
import pickle
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)


def convert(did, session, conn):
    for ctype in ["Section", "FigureContext", "TableContext", "EquationContext", "Combined"]:
        logging.info(f"Indexing {ctype}")
        dataset_pth = os.path.join('/index_dir', did, ctype)
        if os.path.exists(dataset_pth):
            logger.warning(f'Found the dataset directory already created {dataset_pth}. Deleting contents to repopulate')
            shutil.rmtree(dataset_pth)
        os.makedirs(dataset_pth)
        os.makedirs(os.path.join(dataset_pth, 'documents_jsonl'))
        os.makedirs(os.path.join(dataset_pth, 'contexts_jsonl'))

        id_map = {}
        id_context_map = {}
        i = 0
        # Full contexts
        if ctype == "Combined" :
            r = session.query(ObjectContext.id, ObjectContext.content, ObjectContext.pdf_id).filter(ObjectContext.id, ObjectContext.pdf_id == Pdf.id).filter(Pdf.dataset_id == did)
        else:
            r = session.query(ObjectContext.id, ObjectContext.content, ObjectContext.pdf_id).filter(ObjectContext.id, ObjectContext.pdf_id == Pdf.id).filter(Pdf.dataset_id == did).filter(ObjectContext.cls == ctype)
        with open(os.path.join(dataset_pth, 'contexts_jsonl', 'full_contexts.jsonl'), 'w') as wf, open(os.path.join(dataset_pth, 'raw.txt'), 'w') as rawf, open(os.path.join(dataset_pth, 'split_contexts.jsonl'), 'w') as splf:
            for id, content, pdfid in r:
                id_map[id] = pdfid

                example = {'id': i, 'contents': content}
                id_context_map[i] = str(id)
                i += 1
                estr = json.dumps(example)
                wf.write(f'{estr}\n')
                rawf.write(f'{content}\n')
                # Sliding window, stride 64, window 256 (subword splitting downstream, assumption: avg 2 subwords / word)
                spl_content = content.split(' ')
                start = 0
                end = 256
                while end < len(spl_content):
                    example = {'id': i, 'contents': ' '.join(spl_content[start:end])}
                    id_context_map[i] = str(id)
                    i += 1
                    estr = json.dumps(example)
                    splf.write(f'{estr}\n')
                    end += 64
                    start += 64
                final_example = {'id': i, 'contents': ' '.join(spl_content[start:])}
                id_context_map[i] = str(id)
                i += 1
                estr = json.dumps(final_example)
                splf.write(f'{estr}\n')
        with open(os.path.join(dataset_pth, 'context_to_doc.pkl'), 'wb') as wp1, open(os.path.join(dataset_pth, 'id_to_context.pkl'), 'wb') as wp2:
            pickle.dump(id_map, wp1)
            pickle.dump(id_context_map, wp2)

        # Just going to write some SQL here
        q = text('''SELECT pdfs.id, GROUP_CONCAT(object_contexts.content SEPARATOR '\n')
                    FROM object_contexts, pdfs
                    WHERE object_contexts.pdf_id = pdfs.id
                    GROUP BY pdfs.id
                    ''')
        r2 = conn.execute(q)
        i = 0
        id_to_pdf = {}
        with open(os.path.join(dataset_pth, 'documents_jsonl', 'documents.jsonl'), 'w') as wf:
            for pdf_id, content in r2:
                example = {'id': i, 'contents': content}
                id_to_pdf[i] = str(pdf_id)
                i += 1
                estr = json.dumps(example)
                wf.write(f'{estr}\n')
        with open(os.path.join(dataset_pth, 'id_to_pdf.pkl'), 'wb') as wp1:
            pickle.dump(id_to_pdf, wp1)
        logger.info('done writing files')
        logger.info('Starting indexing')
        subprocess.run(['/bin/bash', '-c', f'./index.sh {os.path.join(dataset_pth, "documents_jsonl")}'])
        subprocess.run(['/bin/bash', '-c', f'./index.sh {os.path.join(dataset_pth, "contexts_jsonl")}'])



if __name__ == '__main__':
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    did = os.environ["DATASET_ID"]
    conn = engine.connect()
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    convert(did, session, conn)



