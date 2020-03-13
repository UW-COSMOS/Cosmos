
import click
from distributed.diagnostics.plugin import WorkerPlugin
import os
from ingest.process.detection.src.infer import get_model
import asyncio
import joblib
import yaml

class ProcessPlugin(WorkerPlugin):
    def __init__(self):
        self.cfg_path = '/ingestion/ingest/process/configs/model_config.yaml'
        postprocess_weights_pth = '/ingestion/ingest/process/weights/pp_model_weights.pth'
        self.postprocess_model = joblib.load(postprocess_weights_pth)
        self.classes_pth = '/ingestion/ingest/process/configs/classes.yaml'
        with open(self.classes_pth) as stream:
            self.classes = yaml.load(stream)["classes"]

@click.command()
def dask_setup(worker):
    worker._pending_plugins = [ProcessPlugin()]

    

