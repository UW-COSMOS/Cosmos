import requests
import glob
import uuid
import os
import argparse
from concurrent.futures import ThreadPoolExecutor
import base64
import logging
import time
import tqdm

logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

def run_request(payload):
    filename, dataset_id = payload
    with open(filename, 'rb') as rf:
        bstring = base64.b64encode(rf.read()).decode()

        result = requests.post('http://ingestion:8000/preprocess', json={'pdf': bstring, 'dataset_id': dataset_id, 'pdf_name': os.path.basename(filename)})
        logger.info(result)
    return result


def run(directory, bsz):
    did = str(uuid.uuid4())
    logger.info(f'Dataset id generated: {did}')
    filenames = glob.glob(os.path.join(directory, '*.pdf'))
    if len(filenames) == 0:
        logger.error('Empty input directory')
        return None, None
    fsizes = [os.stat(f).st_size for f in filenames]
    fz = zip(filenames, fsizes)
    srt = sorted(fz, key=lambda x: x[1], reverse=True)
    filenames, _ = zip(*srt)
    dids = [did] * len(filenames)
    zipped = list(zip(filenames, dids))
    logger.info('Starting insertion')
    with ThreadPoolExecutor(max_workers=32) as pool:
        resps = list(tqdm(pool.map(run_request, zipped), total=len(zipped)))
    logger.info('Finished')
    return did, resps


def delete(did):
    result = requests.post('http://ingestion:8000/delete', json={'dataset_id':did})
    logger.info(result)
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('directory', help='Path to pdf directory')
    args = parser.parse_args()
    bsz = int(os.environ['BSZ'])
    stime = time.time()
    run(args.directory, bsz)
    time_up = time.time() - stime
    logger.info(f'TOTAL TIME UP: {time_up} seconds')

    

