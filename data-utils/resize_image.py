from PIL import Image
from argparse import ArgumentParser
import os
from tqdm import tqdm
import xml.etree.ElementTree as ET

def resize_image(path, new_h):
	im = Image.open(path)
	w,h = im.size
	ratio = float(new_h)/h
	new_w = round(ratio*w)
	im = im.resize((new_w, new_h), resample = Image.LANCZOS)
	im.save(f"{path.split('.')[0]}.jpg", "JPEG")
	return ratio

def rewrite_annotation(path, ratio):
	tree = ET.parse(path)
	root = tree.getroot()
	for obj in root.findall("object"):
		for coord in obj.find("bndbox"):
			new_coord = round(float(coord.text)*ratio)
			coord.text = str(int(new_coord))
	tree.write(path)
			

def get_image_ids(path):
	file_names = os.listdir(path)
	identifiers = 	[os.path.splitext(name)[0] for name in file_names]
	return identifiers


def main(img_dir,anno_dir, height):
	image_ids = get_image_ids(img_dir)
	for image_id in tqdm(image_ids):
		image_path = os.path.join(img_dir, f"{image_id}.png")
		anno_path = os.path.join(anno_dir, f"{image_id}.xml")
		ratio = resize_image(image_path, height)
		rewrite_annotation(anno_path, ratio)

if __name__ == "__main__":
	parser = ArgumentParser(description="resize a set of images and rewrite their associated xml files")
	parser.add_argument("img_dir", type=str)
	parser.add_argument("anno_dir", type=str)
	parser.add_argument("height", type=int)
	args = parser.parse_args()
	main(args.img_dir, args.anno_dir, args.height)

