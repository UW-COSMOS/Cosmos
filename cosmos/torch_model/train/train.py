"""
Training helper class
Takes a model, dataset, and training paramters
as arguments
"""
import torch
from torch import nn
from os.path import join, isdir
from os import mkdir
import os
from torch import optim
from torch.utils.data import DataLoader, WeightedRandomSampler
from tqdm import tqdm
from torch_model.train.anchor_targets.head_target_layer import HeadTargetLayer
from functools import partial
from tensorboardX import SummaryWriter


def unpack_cls(cls_dict, gt_label):
    label = cls_dict[gt_label]
    return torch.tensor([label])


def prep_gt_boxes(boxes, device):
    boxes = [box.reshape(1,-1, 4).float().to(device) for box in boxes]
    return boxes


class TrainerHelper:
    def __init__(self, model, train_set, val_set, params,device):
        """
        Initialize a trainer helper class
        :param model: a MMFasterRCNN model
        :param dataset: a GTDataset inheritor to load data from
        :param params: a dictionary of training specific parameters
        """
        self.model = model.to(device)
        self.train_set, self.val_set = train_set, val_set
        self.params = params
        self.cls = dict([(val, idx) for (idx, val) in enumerate(model.cls_names)])
        #self.weight_vec = train_set.get_weight_vec(model.cls_names)
        self.device = device
        weights = self.detect_weights(params["SAVE_DIR"])
        #if weights is not None:
          #  self.model.load_state_dict(torch.load(weights))

        if params["USE_TENSORBOARD"]:
            self.writer = SummaryWriter()
        self.head_target_layer = HeadTargetLayer(
                                     ncls=len(model.cls_names)).to(device)

                                     

    def detect_weights(self,weights_dir):
        ls = os.listdir(weights_dir)
        if len(ls) == 0:
            return
        path = join(weights_dir, ls[len(ls) - 1])

    def train(self):
        optimizer = optim.Adam(self.model.parameters(), 
                              lr=self.params["LEARNING_RATE"],
                              weight_decay=self.params["WEIGHT_DECAY"])
                           
        self.model.train(mode=True)
        iteration = 0
        for epoch in tqdm(range(int(self.params["EPOCHS"])),desc="epochs", leave=False):
            tot_cls_loss = 0.0
            train_loader = DataLoader(self.train_set,
                            batch_size=int(self.params["BATCH_SIZE"]),
                            collate_fn=self.train_set.collate,
                            num_workers=int(self.params["BATCH_SIZE"]*2))
         
            for batch in tqdm(train_loader, desc="training"):
                # print(batch)  
                optimizer.zero_grad()
                windows = batch.neighbor_windows.to(self.device)
                radii = batch.neighbor_radii.to(self.device)
                angles = batch.neighbor_angles.to(self.device)
                ex = batch.center_windows.to(self.device)
                colors = batch.colorfulness.to(self.device)
                gt_cls = batch.labels.to(self.device)
                batch_cls_scores = []
                for i in range(windows.shape[0]):
                  windows_sub = windows[i]
                  ex_sub = ex[i].unsqueeze(0)
                  radii_sub = radii[i].reshape(-1,1)
                  angles_sub = angles[i].reshape(-1,1)
                  colors_sub = colors[i].reshape(-1,1)
                  rois, cls_scores= self.model(ex_sub, windows_sub,radii_sub,angles_sub,colors_sub, batch.center_bbs, self.device)
                  batch_cls_scores.append(cls_scores)
                batch_cls_scores = torch.cat(batch_cls_scores)
                loss = self.head_target_layer(batch_cls_scores, gt_cls.reshape(-1).long(), self.device)
                tot_cls_loss += float(loss)
                loss.backward()
                nn.utils.clip_grad_value_(self.model.parameters(), 5)
                optimizer.step()
            if epoch % self.params["CHECKPOINT_PERIOD"] == 0:
                name = f"model_{epoch}.pth"
                path = join(self.params["SAVE_DIR"], name)
                if not isdir(self.params["SAVE_DIR"]):
                    mkdir(self.params["SAVE_DIR"])
                torch.save(self.model.state_dict(), path)
            del train_loader
            optimizer.zero_grad()
            self.validate(iteration)
            self.writer.add_scalar("train_cls_loss", tot_cls_loss / len(self.train_set), iteration)
            iteration += 1

    def validate(self, iteration=0, to_tensorboard=True):
        loader = DataLoader(self.val_set,
                            batch_size=1,
                            collate_fn=self.val_set.collate,
                            num_workers=3)
        tot_cls_loss = 0.0
        self.model.eval()
        def train_bn(m):
            if type(m) == torch.nn.BatchNorm2d:
                m.train()

        self.model.apply(train_bn)
        for batch in tqdm(loader, desc="validation"):
          windows = batch.neighbor_windows.to(self.device)
          ex = batch.center_windows.to(self.device)
          colors = batch.colorfulness.to(self.device)
          radii = batch.neighbor_radii.to(self.device)
          angles = batch.neighbor_angles.to(self.device)
          gt_cls = batch.labels.to(self.device)
          for i in range(windows.shape[0]):
            windows_sub = windows[i]
            ex_sub = ex[i].unsqueeze(0)
            radii_sub = radii[i].reshape(-1,1)
            angles_sub = angles[i].reshape(-1,1)
            colors_sub = colors[i].reshape(-1,1)
            rois, cls_scores= self.model(ex_sub, windows_sub, radii_sub, angles_sub,colors_sub,batch.center_bbs, self.device)
            cls_loss = self.head_target_layer(cls_scores, gt_cls.reshape(-1).long(), self.device)
            tot_cls_loss += float(cls_loss)
        if to_tensorboard:
                self.output_batch_losses(
                                 tot_cls_loss/len(self.val_set),
                                 iteration)
        self.model.train()
        return tot_cls_loss/len(self.val_set)



    def output_batch_losses(self,  cls_loss,iter ):
        """
        output either by priting or to tensorboard
        :param rpn_cls_loss:
        :param rpn_bbox_loss:
        :param cls_loss:
        :param bbox_loss:
        :return:
        """
        if self.params["USE_TENSORBOARD"]:
            vals = {
                "cls_loss": cls_loss,
            }
            for key in vals:
                self.writer.add_scalar(key, vals[key], iter)
        print(f"  head_cls_loss: {cls_loss}")


def check_grad(model):
    flag = False
    for param in model.parameters():
        if not(param.grad is None):
            if not(param.grad.data.sum() == 0):
                flag = True
    return flag


def save_weights(model):
    save = {}
    for key in model.state_dict():
        save[key] = model.state_dict()[key].clone()
    return save


def check_weight_update(old, new):
    flag = False
    for key in old.keys():
        if not (old[key] == new[key]).all():
            flag = True
    return flag
