import click
from distributed.diagnostics.plugin import WorkerPlugin
import os
from ingest.process.detection.src.infer import get_model

class DetectPlugin(WorkerPlugin):
    def __init__(self):
        self.model_config = os.environ.get("MODEL_CONFIG")
        weights_pth = os.environ.get("WEIGHTS_PTH")
        self.device_str = os.environ.get("DEVICE")
        self.model = get_model(self.model_config, weights_pth, self.device_str)

@click.command()
def dask_setup(worker):
    worker._pending_plugins = [DetectPlugin()]

    

