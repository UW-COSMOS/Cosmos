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
from argparse import ArgumentParser

parser = ArgumentParser("train the model")
parser.add_argument("img_dir", type=str, default="images")
parser.add_argument("xml_dir", type=str, default="annotations")
parser.add_argument("proposals_dir", type=str, default="proposals")
parser.add_argument("model_config",type=str, default="model_config.yaml")
parser.add_argument("train_config", type=str, default="train_config.yaml")

args = parser.parse_args()

cfg = ConfigManager(args.model_config)
model = MMFasterRCNN(cfg)
loader = XMLLoader(args.img_dir, args.xml_dir,
        args.proposals_dir,
        cfg.CC_LAYER.WARPED_SIZE,
        "png")
train_params = None
with open(args.train_config) as fh:
    train_params = yaml.load(fh)
device = torch.device("cuda")
trainer = TrainerHelper(model,
                        loader,
                        train_params,
                        device)
trainer.train()
