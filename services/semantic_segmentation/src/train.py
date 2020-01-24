"""
This example is largely adapted from https://github.com/pytorch/examples/blob/master/imagenet/main.py
"""
import argparse
import os
import random
from collections import OrderedDict
import glob
from PIL import Image
import json
from tqdm import tqdm
import random

import xmltodict
import torch
import torch.backends.cudnn as cudnn
import torch.nn.parallel
import torch.nn.functional as F
import torchvision.transforms.functional as TF
import torch.optim as optim
import torch.optim.lr_scheduler as lr_scheduler
import torch.utils.data
import torch.utils.data.distributed

import torchvision.transforms as transforms
import torchvision.models.segmentation as models
import torchvision.datasets as datasets

from pytorch_lightning.logging import CometLogger
import pytorch_lightning as pl


# pull out resnet names from torchvision models
MODEL_NAMES = sorted(
    name for name in models.__dict__
    if name.islower() and not name.startswith("__") and callable(models.__dict__[name])
)


class GeosciencesDataset(datasets.VisionDataset):
    def __init__(self,
                 root,
                 transform=None,
                 target_transform=None,
                 transforms=None):
        super(GeosciencesDataset, self).__init__(root, transforms, transform, target_transform)
        self.images = []
        self.masks = []
        for x in glob.glob(os.path.join(root, 'images', '*.png')):
            self.images.append(x)
        for x in glob.glob(os.path.join(root, 'masks', '*.png')):
            self.masks.append(x)

        assert (len(self.images) == len(self.masks))

    def __getitem__(self, index):
        """
        Args:
            index (int): Index

        Returns:
            tuple: (image, target) where target is the image segmentation.
        """
        img = Image.open(self.images[index]).convert('RGB')
        target = Image.open(self.masks[index])

        if self.transforms is not None:
            img, target = self.transforms(img, target)

        return img, target


    def __len__(self):
        return len(self.images)

    @classmethod
    def preprocess_annotations(cls, root):
        #ind = 1
        #cls_inds = {}
        for annotation_f in tqdm(glob.glob(os.path.join(root, 'annotations', '*'))):
            with open(annotation_f, 'r') as rf:
                xml = rf.read()
                o = xmltodict.parse(xml)
                annotation = json.dumps(o)
                image_name = o['annotation']['filename']
                img = Image.open(os.path.join(root, 'images', image_name))
                img_tens = transforms.ToTensor()(img)
                _, h, w = img_tens.shape
                mask = torch.zeros((h, w), dtype=torch.int)
                objs = o['annotation']['object']
                for obj in objs:
                    #cls = obj['name']
                    #if cls not in cls_inds:
                    #    cls_inds[cls] = ind
                    #cls_ind = cls_inds[cls]
                    bbox = obj['bndbox']
                    xmin, ymin, xmax, ymax = [int(x) for x in [bbox['xmin'], bbox['ymin'], bbox['xmax'], bbox['ymax']]]
                    mask[ymin:ymax, xmin:xmax] = 1
                pil_mask = transforms.ToPILImage()(mask)
                pil_mask.save(os.path.join(root, 'masks', image_name))

                
class SegmentationModel(pl.LightningModule):

    def __init__(self, hparams):
        super(SegmentationModel, self).__init__()
        self.hparams = hparams
        self.model = models.deeplabv3_resnet101(pretrained=False, num_classes=2)

    def training_step(self, batch, batch_idx):
        images, target = batch
        output = self.model(images)['out']
        loss_val = F.cross_entropy(output, target)
        acc1 = self.__accuracy(output, target, topk=[1])[0]

        # in DP mode (default) make sure if result is scalar, there's another dim in the beginning
        if self.trainer.use_dp or self.trainer.use_ddp2:
            loss_val = loss_val.unsqueeze(0)
            acc1 = acc1.unsqueeze(0)

        tqdm_dict = {'train_loss': loss_val}
        output = OrderedDict({
            'loss': loss_val,
            'acc1': acc1,
            'progress_bar': tqdm_dict,
            'log': tqdm_dict
        })

        return output

    def validation_step(self, batch, batch_idx):
        images, target = batch
        output = self.model(images)['out']
        if random.random() < 0.1:
            _, pred = output[0].squeeze().topk(1, 0)
            pred = pred.cpu().detach()
            pred[pred==1] = 150
            pred = transforms.ToPILImage(mode='L')(pred.type(torch.uint8))
            inp = transforms.ToPILImage()(images[0].squeeze().cpu().detach().type(torch.float))
            out = target[0].squeeze().cpu().detach().type(torch.uint8)
            out[out==1] = 150
            out = transforms.ToPILImage(mode='L')(out)
            self.logger.experiment.log_image(pred, name='Output', step=batch_idx)
            self.logger.experiment.log_image(inp, name='Input', step=batch_idx)
            self.logger.experiment.log_image(out, name='Target', step=batch_idx)
        loss_val = F.cross_entropy(output, target)
        acc1 = self.__accuracy(output, target, topk=[1])[0]

        # in DP mode (default) make sure if result is scalar, there's another dim in the beginning
        if self.trainer.use_dp or self.trainer.use_ddp2:
            loss_val = loss_val.unsqueeze(0)
            acc1 = acc1.unsqueeze(0)

        output = OrderedDict({
            'val_loss': loss_val,
            'val_acc1': acc1,
        })

        return output

    def validation_end(self, outputs):

        tqdm_dict = {}

        for metric_name in ["val_loss", "val_acc1"]:
            metric_total = 0

            for output in outputs:
                metric_value = output[metric_name]

                # reduce manually when using dp
                if self.trainer.use_dp or self.trainer.use_ddp2:
                    metric_value = torch.mean(metric_value)

                metric_total += metric_value

            tqdm_dict[metric_name] = metric_total / len(outputs)

        result = {'progress_bar': tqdm_dict, 'log': tqdm_dict, 'val_loss': tqdm_dict["val_loss"]}
        return result

    @classmethod
    def __accuracy(cls, output, target, topk=(1,)):
        """Computes the accuracy over the k top predictions for the specified values of k"""
        with torch.no_grad():
            maxk = max(topk)
            batch_size = target.size(0)

            _, pred = output.topk(maxk, 1, True, True)
            pred = torch.transpose(pred, 0, 1)
            orig_shape = pred.shape
            pred = pred.view(orig_shape[0], orig_shape[1], -1)
            num_pix = pred.shape[2]
            target = target.view(1, target.shape[0], -1).expand_as(pred)

            correct = pred.eq(target)

            res = []
            for k in topk:
                correct_k = correct[:k].view(-1).float().sum(0, keepdim=True)
                res.append(correct_k.mul_(100.0 / (batch_size * num_pix)))
            return res

    def configure_optimizers(self):
        optimizer = optim.SGD(
            self.parameters(),
            lr=self.hparams.lr,
            momentum=self.hparams.momentum,
            weight_decay=self.hparams.weight_decay
        )
        scheduler = lr_scheduler.ExponentialLR(optimizer, gamma=0.1)
        return [optimizer], [scheduler]

    @pl.data_loader
    def train_dataloader(self):
        def train_transform(input, target):
            
            angle = transforms.RandomRotation.get_params([-20, 20])
            input = TF.rotate(input, angle)
            target = TF.rotate(target, angle)

            resize_crop_params = transforms.RandomResizedCrop.get_params(input, (0.08, 1.0), (3. / 4., 4. / 3.))
            input = TF.resized_crop(input, *resize_crop_params, (520, 520))
            target = TF.resized_crop(target, *resize_crop_params, (520, 520))

            input = TF.to_tensor(input)
            target = TF.to_tensor(target).long().squeeze()

            normalize = transforms.Normalize(
                mean=[0.485, 0.456, 0.406],
                std=[0.229, 0.224, 0.225],
            )
            input = normalize(input)

            return input, target


        train_dir = os.path.join(self.hparams.data_path, 'train')
        train_dataset = GeosciencesDataset(
            train_dir,
            transforms=train_transform)

        if self.use_ddp:
            train_sampler = torch.utils.data.distributed.DistributedSampler(train_dataset)
        else:
            train_sampler = None

        train_loader = torch.utils.data.DataLoader(
            dataset=train_dataset,
            batch_size=self.hparams.batch_size,
            shuffle=(train_sampler is None),
            num_workers=0,
            sampler=train_sampler
        )
        return train_loader

    @pl.data_loader
    def val_dataloader(self):
        def val_transform(input, target):

            crop_params = transforms.RandomCrop.get_params(input, (520, 520))
            input = TF.crop(input, *crop_params)
            target = TF.crop(target, *crop_params)

            input = TF.to_tensor(input)
            target = TF.to_tensor(target).long().squeeze()

            normalize = transforms.Normalize(
                mean=[0.485, 0.456, 0.406],
                std=[0.229, 0.224, 0.225],
            )
            input = normalize(input)

            return input, target

        val_dir = os.path.join(self.hparams.data_path, 'val')
        val_loader = torch.utils.data.DataLoader(
            GeosciencesDataset(val_dir, transforms=val_transform),
            batch_size=self.hparams.batch_size,
            shuffle=False,
            num_workers=0,
        )
        return val_loader

    @staticmethod
    def add_model_specific_args(parent_parser):  # pragma: no cover
        parser = argparse.ArgumentParser(parents=[parent_parser])
        print(MODEL_NAMES)
        parser.add_argument('-a', '--arch', metavar='ARCH', default='resnet18', choices=MODEL_NAMES,
                            help='model architecture: ' +
                                 ' | '.join(MODEL_NAMES) +
                                 ' (default: resnet18)')
        parser.add_argument('--epochs', default=90, type=int, metavar='N',
                            help='number of total epochs to run')
        parser.add_argument('--seed', type=int, default=None,
                            help='seed for initializing training. ')
        parser.add_argument('-b', '--batch-size', default=256, type=int,
                            metavar='N',
                            help='mini-batch size (default: 256), this is the total '
                                 'batch size of all GPUs on the current node when '
                                 'using Data Parallel or Distributed Data Parallel')
        parser.add_argument('--lr', '--learning-rate', default=0.1, type=float,
                            metavar='LR', help='initial learning rate', dest='lr')
        parser.add_argument('--momentum', default=0.9, type=float, metavar='M',
                            help='momentum')
        parser.add_argument('--wd', '--weight-decay', default=1e-4, type=float,
                            metavar='W', help='weight decay (default: 1e-4)',
                            dest='weight_decay')
        parser.add_argument('--pretrained', dest='pretrained', action='store_true',
                            help='use pre-trained model')
        return parser


def get_args():
    parent_parser = argparse.ArgumentParser(add_help=False)
    parent_parser.add_argument('--data-path', metavar='DIR', type=str,
                               help='path to dataset')
    parent_parser.add_argument('--save-path', metavar='DIR', default=".", type=str,
                               help='path to save output')
    parent_parser.add_argument('--gpus', type=int, default=1,
                               help='how many gpus')
    parent_parser.add_argument('--distributed-backend', type=str, default='dp', choices=('dp', 'ddp', 'ddp2'),
                               help='supports three options dp, ddp, ddp2')
    parent_parser.add_argument('--use-16bit', action='store_true',
                               help='if true uses 16 bit precision')
    parent_parser.add_argument('-e', '--evaluate', dest='evaluate', action='store_true',
                               help='evaluate model on validation set')

    parser = SegmentationModel.add_model_specific_args(parent_parser)
    return parser.parse_args()


def main(hparams):
    model = SegmentationModel(hparams)
    if hparams.seed is not None:
        random.seed(hparams.seed)
        torch.manual_seed(hparams.seed)
        cudnn.deterministic = True
    comet_logger = CometLogger(
                    api_key=os.environ["COMET_KEY"],
                    workspace=os.environ["COMET_WORKSPACE"],
                    experiment_name=os.environ["COMET_EXPERIMENT_NAME"],
                    project_name=os.environ["COMET_PROJECT"],
                    )

    trainer = pl.Trainer(
        default_save_path=hparams.save_path,
        gpus=hparams.gpus,
        max_nb_epochs=hparams.epochs,
        distributed_backend=hparams.distributed_backend,
        use_amp=hparams.use_16bit,
        logger=comet_logger
    )
    if hparams.evaluate:
        trainer.run_evaluation()
    else:
        trainer.fit(model)


if __name__ == '__main__':
    main(get_args())
