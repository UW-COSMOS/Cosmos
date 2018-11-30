import mrcnn.model as modellib
from config import PageConfig
from argparse import ArgumentParser
from dataset import PageDataset

parser = ArgumentParser(description="train a Mask-RCNN model")
parser.add_argument("save_dir", type=str, help="weights loading/saving directory")
parser.add_argument("epochs", type=int, help="total number of epochs")
parser.add_argument("data_dir", type=str, help="path to training VOC set")
args = parser.parse_args()


data_train = PageDataset()
data_train.load_page("train")
data_train.prepare()
data_val = PageDataset()
data_val.load_page(args.data_dir,"val")
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
