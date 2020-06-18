"""
Helpers for getting PDFs ready for ingestion
"""
import uuid
import glob
import os
import base64

def create_objs(payload):
    filename, dataset_id = payload
    with open(filename, 'rb') as rf:
        bstring = base64.b64encode(rf.read()).decode()

def prepare_pdf_objs(directory: str, dataset_id=None):
    """
    :param directory: Directory to process
    :param dataset_id: The dataset id. This allows you to group datasets together.
    :return: 
    """
    if dataset_id is None:
        dataset_id = str(uuid.uuid4())

    files = glob.glob(os.path.join(directory, '*.pdf'))
    if len(files) == 0:
        print('Empty input directory')
        return
    objs = []
    for filename in files:
        with open(filename, 'rb') as rf:
            bstring = base64.b64encode(rf.read()).decode()
        objs.append({'pdf': bstring, 'dataset_id': dataset_id, 'pdf_name': os.path.basename(filename)})
    return objs

