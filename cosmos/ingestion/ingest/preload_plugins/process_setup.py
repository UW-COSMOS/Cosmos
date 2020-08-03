import click
from distributed.diagnostics.plugin import WorkerPlugin
import os
import joblib
import yaml

class ProcessPlugin(WorkerPlugin):
    def __init__(self, cfg_path=os.environ.get("MODEL_CONFIG"),
                       weights_path=os.environ.get("PP_WEIGHTS_PTH"),
                       classes_path=os.environ.get("CLASSES_PTH")):
        self.cfg_path = cfg_path
        postprocess_weights_pth = weights_path
        self.postprocess_model = joblib.load(postprocess_weights_pth)
        self.classes_pth = classes_path
        with open(self.classes_pth) as stream:
            self.classes = yaml.load(stream)["classes"]

@click.command()
def dask_setup(worker):
    worker._pending_plugins = [ProcessPlugin()]

    

