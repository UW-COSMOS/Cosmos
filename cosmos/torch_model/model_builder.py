"""
Implements the expected HyperYAML interface
"""
import torch
import click
from hyperyaml.hyperyaml import HyperYaml
from torch_model.model.model import MMFasterRCNN
from torch_model.train.train import TrainerHelper
from torch_model.model.utils.config_manager import ConfigManager
from torch_model.train.data_layer.xml_loader import XMLLoader
from ingestion.ingest_images import ImageDB
import yaml
from os.path import join
def get_img_dir(root):
    return join(root,"images")

def get_anno_dir(root):
    return join(root, "annotations")

def get_proposal_dir(root):
    return join(root, "proposals")

def get_dataset(dir, warped_size, expansion_delta, img_type, partition):
    session, ingest_objs = ImageDB.initialize_and_ingest(get_img_dir(dir),
                                                         get_proposal_dir(dir),
                                                         get_anno_dir(dir),
                                                         warped_size,
                                                         partition,
                                                         expansion_delta)
    dataset = XMLLoader(session, ingest_objs)
    return dataset

class ModelBuilder:
    def __init__(self,val_dir, train_dir, start_config="model_config.yaml", device=torch.device("cuda"), start_train="train_config.yaml"):
        self.config = ConfigManager(start_config)
        warped_size = self.config.WARPED_SIZE
        expansion_delta = self.config.EXPANSION_DELTA
        self.device = device
        with open(start_train) as stream:
            self.train_config = yaml.load(stream)
        self.train_set = get_dataset(train_dir, warped_size, expansion_delta, "png", 'train')
        self.val_set = get_dataset(val_dir, warped_size, expansion_delta, "png", 'val')

    def build(self, params):
        self.build_cfg(params)
        self.build_train_cfg(params)
        model = MMFasterRCNN(self.config)
        # TODO the params object shouldn't have any reason to leak into 
        trainer = TrainerHelper(model, self.train_set, self.val_set,self.train_config, self.device)
        trainer.train()
        loss = trainer.validate()
        return loss

    def build_train_cfg(self, params):
        for key in self.train_config.keys():
            if key in params:
                self.train_config[key] = params[key]

    def build_cfg(self, params):
        print(params)
        for key in params.keys():
            if hasattr(self.config, key):
                setattr(self.config, key, params[key])

@click.command()
@click.argument("hyperopt_config")
@click.argument("max_evals", type=int)
@click.argument("val_dir")
@click.argument("train_dir")
def build_model(hyperopt_config, max_evals, val_dir, train_dir):
    builder = ModelBuilder(val_dir, train_dir)
    hyp = HyperYaml(hyperopt_config,builder, max_evals)
    hyp.run()
    hyp.write("best.yaml")

if __name__ == "__main__":
    build_model()
