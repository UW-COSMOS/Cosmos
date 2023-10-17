from apscheduler.schedulers.background import BackgroundScheduler
import shutil
from datetime import datetime, timedelta
from db.processing_session_types import CosmosSessionJob
from db.db import SessionLocal
from sqlalchemy import select, delete, ScalarResult
from fastapi.logger import logger

SESSION_EXPIRATION = timedelta(hours = 24)

def clean_all_expired_jobs():
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


def init_scheduler():
    scheduler = BackgroundScheduler()
    scheduler.add_job(clean_all_expired_jobs, 'cron', hour="*")
    scheduler.start()

