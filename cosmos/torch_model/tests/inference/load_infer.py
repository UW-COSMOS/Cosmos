from model.model import MMFasterRCNN
from train.data_layer.gt_dataset import GTDataset
from train.data_layer.xml_loader import XMLLoader
from inference.inference import InferenceHelper
import yaml
import torch
from argparse import ArgumentParser

def run(img_path, anno_path):
    device = None
    if torch.cuda.is_available():
        device = torch.device("cuda")
    else:
        device = torch.device("cpu")
    loader = XMLLoader(anno_path, img_path, "jpg")
    dataset = GTDataset(loader)
    model_config = yaml.load(open("model_config.yaml").read())
    print("------- Building Model --------")
    model = MMFasterRCNN(model_config)
    model.load_state_dict(torch.load("weights/model_5.pth", map_location=device))
    inference = InferenceHelper(model, dataset, device)
    inference.get_predictions()



if __name__ == "__main__":
    parser = ArgumentParser(description="run on a PASCAL VOC dataset")
    parser.add_argument('img_path', type=str)
    parser.add_argument('anno_path', type=str)
    args = parser.parse_args()
    run(args.img_path, args.anno_path)

