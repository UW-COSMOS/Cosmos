import mrcnn.model as modellib
from config import PageConfig
from argparse import ArgumentParser
from dataset import PageDataset
from voc_utils import ICDAR_convert

parser = ArgumentParser(description="train a Mask-RCNN model")
parser.add_argument("--save_dir", type=str, default='weights/', help="weights loading/saving directory")
parser.add_argument("--epochs", type=int, help="total number of epochs")
parser.add_argument("--data_dir", type=str, default='data/', help="path to training VOC set")
parser.add_argument('--collapse', type=int, default=0, help='Collapse to ICDAR classes + body text')

args = parser.parse_args()

data_dir = args.data_dir
collapse = bool(args.collapse)
# just in case
if data_dir[-1] != '/':
    data_dir += '/'
data_train = PageDataset('train', data_dir, collapse)
# TODO Generalize how classes gets passed in.
data_train.load_page(classes=list(ICDAR_convert.keys()))
data_train.prepare()
data_val = PageDataset('val', data_dir, collapse)
data_val.load_page(classes=list(ICDAR_convert.keys()))
data_val.prepare()
config = PageConfig()
model = modellib.MaskRCNN(mode="training", config=config,
                          model_dir=args.save_dir)

model_path = model.find_last()
print("reloading wieghts from {}".format(model_path))
model.load_weights(model_path, by_name=True, exclude=['mrcnn_bbox_fc', 'mrcnn_class_logits', 'mrcnn_mask'])

model.train(data_train, data_val, 
            learning_rate=config.LEARNING_RATE, 
            epochs=args.epochs, 
            layers='heads')

