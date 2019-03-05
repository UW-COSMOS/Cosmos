"""
run the model training
on already preprocessed data
Author: Josh McGrath
"""
import torch
from model.model import MMFasterRCNN
from train.train import TrainerHelper
from model.utils.config_manager import ConfigManager
from train.data_layer.xml_loader import XMLLoader
import yaml
import click
from os.path import join


@click.command()
@click.argument("model_config")
@click.argument("train_config")
@click.argument("train_dir")
@click.argument("val_dir")
def train(model_config, train_config, train_dir, val_dir):
	cfg = ConfigManager(model_config)
	model = MMFasterRCNN(cfg)
	train_loader = XMLLoader(join(train_dir, "images"),
			join(train_dir, "annotations"),
			join(train_dir, "proposals"),
			cfg.WARPED_SIZE,
			"png")
	val_loader = XMLLoader(join(val_dir, "images"),
			join(val_dir, "annotations"),
			join(val_dir, "proposals"),
			cfg.WARPED_SIZE,
			"png")

	train_params = None
	with open(train_config) as fh:
			train_params = yaml.load(fh)
	device = torch.device("cuda")
	trainer = TrainerHelper(model,
													train_loader,
													val_loader,
													train_params,
													device)
	trainer.train()


if __name__ == "__main__":
	train()
