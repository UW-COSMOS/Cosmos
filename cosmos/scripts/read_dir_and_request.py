import requests
import glob
import uuid
import os
import argparse
from concurrent.futures import ThreadPoolExecutor
import base64
import logging
import time
from tqdm import tqdm
import time

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()
logger.setLevel(logging.INFO)

def run_request(payload):
    filename, dataset_id = payload
    with open(filename, 'rb') as rf:
        bstring = base64.b64encode(rf.read()).decode()

        result = requests.post('http://ingestion:8000/preprocess', json={'pdf': bstring, 'dataset_id': dataset_id, 'pdf_name': os.path.basename(filename)})
    return result


def run(directory, dataset_id=""):
    if dataset_id == "":
        did = str(uuid.uuid4())
        logger.info(f'Dataset id generated: {did}')
    else:
        did = dataset_id
    d = directory
    directories = [os.path.join(directory, o) for o in os.listdir(directory) if os.path.isdir(os.path.join(directory,o))]
    directories.append(directory)
    processed_subdirs = []
    for ind, subdir in enumerate(directories):
        if ind == 4:
            break
        processed_subdirs.append(subdir)
        print(subdir)
        filenames = glob.glob(os.path.join(subdir, '*.pdf'))
        if len(filenames) == 0:
            logger.error('Empty input directory')
            return None, None
        fsizes = [os.stat(f).st_size for f in filenames]
        fz = zip(filenames, fsizes)
        srt = sorted(fz, key=lambda x: x[1], reverse=True)
        filenames, _ = zip(*srt)
        dids = [did] * len(filenames)
        zipped = list(zip(filenames, dids))
        logger.info('Submitting jobs')
        with ThreadPoolExecutor(max_workers=32) as pool:
            resps = list(tqdm(pool.map(run_request, zipped), total=len(zipped)))
        logger.info('Finished submitting jobs')
        successful_resps = [r for r in resps if r.status_code == 200]
        logger.info(f'There were {len(resps) - len(successful_resps)} failed job submissions')
        time.sleep(2400)
    print(processed_subdirs)
    #pages_dict = {}
    #with tqdm(total=len(successful_resps)) as pbar:
    #    done_count = 0
    #    error_count = 0
    #    prev_done_count = 0
    #    while done_count < len(successful_resps):
    #        done_count = 0
    #        error_count = 0
    #        for resp in successful_resps:
    #            obj = resp.json()
    #            tid = obj['data']['task_id']
    #            url = f'http://ingestion:8000/status/{tid}'
    #            result = requests.get(url)
    #            if result.status_code == 200:
    #                obj = result.json()
    #                status = obj['status']
    #                if status == 'SUCCESS':
    #                    done_count += 1
    #                elif status == 'FAILURE':
    #                    done_count += 1
    #                    error_count += 1
    #            else:
    #                error_count += 1
    #                done_count += 1
    #        if prev_done_count < done_count:
    #            pbar.update(done_count - prev_done_count)
    #            prev_done_count = done_count
    #        time.sleep(10)
    #logger.info(f'Done ingesting. There were {error_count} failures')
    #tids = []
    #for resp in successful_resps:
    #    obj = resp.json()
    #    tid = obj['data']['task_id']
    #    url = f'http://ingestion:8000/status/{tid}'
    #    result = requests.get(url)
    #    if result.status_code == 200:
    #        obj = result.json()
    #        status = obj['status']
    #        if status == 'SUCCESS':
    #            task_result = obj['result']
    #            page_task_ids = task_result['page_tasks']
    #            tids.extend(page_task_ids)
    #logger.info(f'Now monitoring page level jobs')
    #with tqdm(total=len(tids)) as pbar:
    #    done_count = 0
    #    prev_done_count = 0
    #    while done_count < len(tids):
    #        done_count = 0
    #        error_count = 0
    #        for tid in tids:
    #            url = f'http://ingestion:8000/status/{tid}'
    #            result = requests.get(url)
    #            if result.status_code == 200:
    #                obj = result.json()
    #                status = obj['status']
    #                if status == 'SUCCESS':
    #                    done_count += 1
    #                elif status == 'FAILURE':
    #                    done_count += 1
    #                    error_count += 1
    #            else:
    #                error_count += 1
    #                done_count += 1
    #        if prev_done_count < done_count:
    #            pbar.update(done_count - prev_done_count)
    #            prev_done_count = done_count
    #        time.sleep(10)
    #logger.info('Done processing all pages')


def delete(did):
    result = requests.post('http://ingestion:8000/delete', json={'dataset_id':did})
    logger.info(result)
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('directory', help='Path to pdf directory')
    parser.add_argument('dataset_id', default="", help='dataset_id for bookkeeping + organization')
    args = parser.parse_args()
    stime = time.time()
    run(args.directory, args.dataset_id)
    time_up = time.time() - stime
    logger.info(f'TOTAL TIME UP: {time_up} seconds')
