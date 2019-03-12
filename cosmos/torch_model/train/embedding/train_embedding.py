"""
Helper class
for training an Image embedding
Author: Josh McGrath
"""
import torch
from torch.nn import BCEWithLogitsLoss
from torch.utils.data import DataLoader
from torch.optim import Adam
from .embedding_dataset import ImageEmbeddingDataset
from tqdm import tqdm

class EmbeddingTrainer:

    def __init__(self, model,device, db, epochs, train_size, val_size):
        self.model = model
        self.epochs = epochs
        self.db = db
        self.bce_loss = BCEWithLogitsLoss()
        self.train_set, self.val_set = self.build_datasets()

    def train(self):
        loader = DataLoader(self.train_set, batch_size=1, shuffle=True, workers=4)
        optimizer = Adam(self.model.parameters(),
                        lr = 0.001,
                        weight_decay = 0.0005)
        for epoch in  tqdm(self.epochs):
            train_loss = 0.0
            for batch in tqdm(loader):
                optimizer.zero_grad()
                word_vecs = model(batch.words)
                context_vecs = model(batch.contexts)
                preds = torch.sum(word_vecs*context_vecs, dim=1)
                loss = self.bce_loss(preds, batch.labels)
                train_loss += float(loss)/len(self.train_set)
                loss.backward()
                optimizer.step()
            val_loss = self.validate()
            print(f"epoch: {epoch}, val_loss: {val_loss}, train_loss: {train_loss}")


    def validate(self):
        loader = DataLoader(self.val_set, workers=4, batch_size=1, shuffle=True)
        avg_loss = 0.0
        for batch in tqdm(loader):
            word_vecs = model(batch.words)
            context_vecs = model(batch.contexts)
            preds = torch.sum(word_vecs*context_vecs, dim=1)
            loss = self.bce_loss(preds, batch.labels)
            avg_loss += float(loss)/len(self.val_set)
        return avg_loss


    def build_datasets(self):
        pass
