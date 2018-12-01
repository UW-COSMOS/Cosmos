import mrcnn.model as modellib
from config import PageConfig
from argparse import ArgumentParser
from dataset import PageDataset

parser = ArgumentParser(description="train a Mask-RCNN model")
parser.add_argument("save_dir", type=str, default='weights/', help="weights loading/saving directory")
parser.add_argument("epochs", type=int, help="total number of epochs")
parser.add_argument("data_dir", type=str, default='/experiment/pvoc_utils/', help="path to training VOC set")
args = parser.parse_args()

data_dir = args.data_dir
# just in case
if data_dir[-1] != '/':
    data_dir += '/'
data_dir += '{}'
data_train = None
data_val = None
data_train = PageDataset('train', path=args.data_dir)

data_train.load_page()
data_train.prepare()
data_train = PageDataset('val', path=args.data_dir)
data_val.load_page()
data_val.prepare()
config = PageConfig()
model = modellib.MaskRCNN(mode="training", config=config,
                          model_dir=args.save_dir)

model_path = model.find_last()
print(f"reloading wieghts from {model_path}")
model.load_weights(model_path, by_name=True)

model.train(data_train, data_val, 
            learning_rate=config.LEARNING_RATE, 
            epochs=args.epochs, 
            layers='heads')
