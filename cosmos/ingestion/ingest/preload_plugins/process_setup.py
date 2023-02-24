import click
from distributed.diagnostics.plugin import WorkerPlugin
from xgboost import XGBClassifier
import os
import yaml


class ProcessPlugin(WorkerPlugin):
    def __init__(self, cfg_path=os.environ.get("MODEL_CONFIG"),
                       weights_path=os.environ.get("PP_WEIGHTS_PTH"),
                       classes_path=os.environ.get("CLASSES_PTH")):
        self.cfg_path = cfg_path
        postprocess_weights_pth = weights_path
        self.postprocess_model = XGBClassifier()
        self.postprocess_model.load_model(postprocess_weights_pth)
        self.classes_pth = classes_path
        with open(self.classes_pth) as stream:
            self.classes = yaml.load(stream, Loader=yaml.FullLoader)["CLASSES"]


@click.command()
def dask_setup(worker):
    worker._pending_plugins = [ProcessPlugin()]



