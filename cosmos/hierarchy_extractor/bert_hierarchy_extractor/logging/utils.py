from comet_ml import Experiment
import requests

def log_metrics(experiment: Experiment, metrics: dict, step: int):
    for metric in metrics:
        m = metrics[metric]
        experiment.log_metric(metric, m, step)
