"""
run the model training
on already preprocessed data
Author: Josh McGrath
"""
import torch
from torch_model.model.model import MMFasterRCNN
from torch_model.train.train import TrainerHelper
from torch_model.model.utils.config_manager import ConfigManager
from torch_model.train.data_layer.xml_loader import XMLLoader
import yaml
import click
from os.path import join
from torch_model.train.embedding.embedding_dataset import ImageEmbeddingDataset
from torch_model.train.embedding.train_embedding import EmbeddingTrainer
from ingestion.ingest_images import ImageDB

classes = None
with open("classes.yaml") as stream:
  classes  = yaml.load(stream)["classes"]

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
    dataset = XMLLoader(session, ingest_objs, classes)
    embedding_dataset = ImageEmbeddingDataset(session, ingest_objs, classes)
    return dataset, embedding_dataset



@click.command()
@click.argument("model_config")
@click.argument("train_config")
@click.argument("train_dir")
@click.argument("val_dir")
def train(model_config, train_config, train_dir, val_dir):
  cfg = ConfigManager(model_config)
  model = MMFasterRCNN(cfg)
  train_loader, embedding_train_loader = get_dataset(train_dir,
      cfg.WARPED_SIZE,
      cfg.EXPANSION_DELTA,
      "png",
      "train")
  val_loader, embedding_val_loader = get_dataset(val_dir,
      cfg.WARPED_SIZE,
      cfg.EXPANSION_DELTA,
      "png",
      "val")

  train_params = None
  with open(train_config) as fh:
      train_params = yaml.load(fh)
  device = torch.device("cuda")
  model.to(device)
  emb_trainer = EmbeddingTrainer(model.featurizer,
      model.embedder,
      device,
      3,
      embedding_train_loader,
      embedding_val_loader)
  print("===== Training Embeddings =====")
  emb_trainer.train()
  print("==== Done Training Embeddings ====")
  del emb_trainer
  trainer = TrainerHelper(model,
                          train_loader,
                          val_loader,
                          train_params,
                          device)
  trainer.train()


if __name__ == "__main__":
  train()
