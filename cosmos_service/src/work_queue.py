from fastapi.logger import logger
from typing import List
import asyncio
import sys

queue = asyncio.Queue()
workers : List[asyncio.Task] = None

COSMOS_SCRIPT = 'process.py'

# if a task fails due to the GPU being out of memory, wait for a while and then try again
OOM_SLEEP_TIME = 30
OOM_ERROR_EXIT_CODE = 2

async def _cosmos_worker(work_queue: asyncio.Queue):
    """
    Cosmos worker process. Continually poll from the work queue for new parameters to the pipeline,
    and run the cosmos pipeline in a separate process. A separate process is necessary to avoid I/O 
    blocking issues in Python's async framework
    """
    while True:
        (job_output_dir, job_id, compress_images) = await work_queue.get()
        proc = await asyncio.create_subprocess_exec(sys.executable, COSMOS_SCRIPT, job_output_dir, job_id, str(compress_images))
        result = await proc.wait()
        queue.task_done()
        
        if result == OOM_ERROR_EXIT_CODE:
            await asyncio.sleep(OOM_SLEEP_TIME)
            await queue.put((job_output_dir, job_id, compress_images))


def setup_workers(worker_count):
    global workers
    """
    Initialize FastAPI and add variables
    """
    logger.info(f"Creating {worker_count} work queues for COSMOS processing")
    workers = [asyncio.create_task(_cosmos_worker(queue)) for _ in range(worker_count)]
