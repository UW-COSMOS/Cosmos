from celery import Celery
import logging
from celery.utils.log import get_task_logger

CELERY_BROKER_URL = "amqp://admin:mypass@rabbit:5672"
CELERY_RESULT_BACKEND = "redis://redis:6379/0"

celery = Celery('workerA', broker=CELERY_BROKER_URL, backend=CELERY_RESULT_BACKEND)
logger = get_task_logger(__name__)

@celery.task
def sub_nums(a, b):
    logger.debug("subtracting")
    return int(a)-int(b)
