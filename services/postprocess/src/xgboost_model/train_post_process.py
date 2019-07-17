from xgboost_model.featurizer import load_data
from xgboost_model.model import PostProcessTrainer
import yaml
from argparse import ArgumentParser
import os

from xgboost import XGBClassifier

with open("classes.yaml") as stream:
    classes  = yaml.load(stream)["classes"]

if __name__ == '__main__':
	parser = ArgumentParser(description="Train the PostProcessing model")
	parser.add_argument('-i', "--input", default='./', help="Input directory")
	parser.add_argument('-l', "--log_dir", required=False)
	
	args = parser.parse_args()
	input_path = args.input
	log_dir = args.log_dir

	print("Featuring Training Dataset")
	train_x, train_y = load_data(os.path.join(input_path, 'train_dir'), classes)
	print("Featuring Validation Dataset")
	val_x, val_y = load_data(os.path.join(input_path, 'val_dir'), classes)
	print("Dataset featurized")
	model = XGBClassifier()
	
	print("Training model")
	trainer = PostProcessTrainer(model, train_x, train_y, val_x, val_y, log_dir=log_dir)

	trainer.train()

	print("Training completed")
