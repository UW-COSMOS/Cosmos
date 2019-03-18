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

    def __init__(self,featurizer ,model,device, epochs,train_set, val_set):
        self.model = model
        self.featurizer = featurizer
        self.device = device
        self.epochs = epochs
        self.bce_loss = BCEWithLogitsLoss()
        self.train_set = train_set
        self.val_set = val_set

    def train(self):
        loader = DataLoader(self.train_set,
            batch_size=12,
            shuffle=True,
            num_workers=4,
            collate_fn=self.train_set.collate)
        optimizer = Adam(self.model.parameters(),
                        lr = 0.0005,
                        weight_decay = 0.00005)
        for epoch in  tqdm(range(self.epochs)):
            train_loss = 0.0
            for batch in tqdm(loader):
                N = batch.neighbor_windows.shape[0]
                radii_vec = torch.zeros(N,1).to(self.device)
                angles_vec = torch.zeros(N,1).to(self.device)
                optimizer.zero_grad()
                word_maps = self.featurizer(batch.center_windows.to(self.device), self.device)
                word_vecs = self.model(word_maps,radii_vec, angles_vec)
                neighbor_windows = batch.neighbor_windows.to(self.device)
                neighbor_radii = batch.neighbor_radii.to(self.device)
                neighbor_angles = batch.neighbor_angles.to(self.device)
                for i in range(neighbor_windows.shape[0]):
                  neighbor_windows_sub = neighbor_windows[i]
                  context_maps = self.featurizer(neighbor_windows_sub,self.device)
                  context_vecs = self.model(context_maps, neighbor_radii[i].reshape(-1,1), neighbor_angles[i].reshape(-1,1))
                  preds = torch.sum(word_vecs[i]*context_vecs, dim=1)
                  loss = self.bce_loss(preds, batch.labels[i].expand(neighbor_windows_sub.shape[0]).to(self.device))
                  train_loss += float(loss)/len(self.train_set)
                loss.backward()
                optimizer.step()
            val_loss = self.validate()
            print(f"epoch: {epoch}, val_loss: {val_loss}, train_loss: {train_loss}")


    def validate(self):
        loader = DataLoader(self.val_set, num_workers=4, batch_size=1, collate_fn=self.val_set.collate)
        avg_loss = 0.0
        for batch in tqdm(loader):
            N = batch.neighbor_windows.shape[0]
            radii_vec = torch.zeros(N,1).to(self.device)
            angles_vec = torch.zeros(N,1).to(self.device)
            word_maps = self.featurizer(batch.center_windows.to(self.device), self.device)
            word_vecs = self.model(word_maps, radii_vec, angles_vec)
            neighbor_windows = batch.neighbor_windows.to(self.device)
            neighbor_radii = batch.neighbor_radii.to(self.device)
            neighbor_angles = batch.neighbor_angles.to(self.device)
            for i in range(neighbor_windows.shape[0]):
                neighbor_windows_sub = neighbor_windows[i]
                context_maps = self.featurizer(neighbor_windows_sub,self.device)
                context_vecs = self.model(context_maps, neighbor_radii[i].reshape(-1,1), neighbor_angles.reshape(-1,1))
                preds = torch.sum(word_vecs[i]*context_vecs, dim=1)
                loss = self.bce_loss(preds, batch.labels[i].expand(neighbor_windows_sub.shape[0]).to(self.device))
                avg_loss += float(loss)/len(self.val_set)
        return avg_loss 
