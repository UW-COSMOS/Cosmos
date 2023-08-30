from rocketry import Rocketry
from rocketry.conds import every
import shutil
from datetime import datetime, timedelta
from db.processing_session_types import CosmosSessionJob
from db.db import SessionLocal
from sqlalchemy import select, delete, ScalarResult
from fastapi.logger import logger


scheduler = Rocketry(config={"task_execution": "async"})


CLEAN_JOB_FREQUENCY = "1 hour"
SESSION_EXPIRATION = timedelta(hours = 24)


@scheduler.task(every(CLEAN_JOB_FREQUENCY))
async def clean_all_expired_jobs():
    """Find every cosmos pipeline job that started over SESSION_EXPIRATION hours ago,
    clean up its working directory, and delete its entry from the database
    """
    expiration_time = datetime.now() - SESSION_EXPIRATION
    with SessionLocal() as session:
        expired_jobs = select(CosmosSessionJob).where(CosmosSessionJob.created < expiration_time)
        to_delete : ScalarResult = session.scalars(expired_jobs)
        logger.info(f"Clearing {len(list(to_delete))} expired COSMOS jobs")

        temporary_dirs = [job.output_dir for job in to_delete]

        delete_jobs = delete(CosmosSessionJob).where(CosmosSessionJob.created < expiration_time)
        session.execute(delete_jobs)
        session.commit()


    for dir in temporary_dirs:
        shutil.rmtree(dir)

