
import click
from distributed.diagnostics.plugin import WorkerPlugin
import os
from ingest.process.detection.src.infer import get_model
import asyncio

class DetectPlugin(WorkerPlugin):
    def __init__(self, model_config=None, weights_pth=None, device_str=None, keep_bytes=True):
        if model_config is None or weights_pth is None or device_str is None:
            self.model_config = '/ingestion/ingest/process/configs/model_config.yaml'
            weights_pth = '/ingestion/ingest/process/weights/model_weights.pth'
            self.device_str = os.environ.get('DEVICE')
        else:
            self.model_config = model_config
            self.device_str = device_str
        self.keep_bytes = keep_bytes
        self.model = get_model(self.model_config, weights_pth, self.device_str)

@click.command()
def dask_setup(worker):
    worker._pending_plugins = [DetectPlugin()]

    

