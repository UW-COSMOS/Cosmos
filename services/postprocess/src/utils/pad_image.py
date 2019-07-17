from PIL import ImageOps
def pad_image(im, size):
	w, h = im.size
	d_w = size - w
	d_h = size - h
	if d_h < 0 or d_w < 0:
		raise Exception("negative pad")
	padding = (0,0,d_w, d_h)
	im_2 = ImageOps.expand(im, padding, fill="#fff")
	return im_2


if __name__ == "__main__":
	import os
	from argparse import ArgumentParser
	from PIL import Image
	from tqdm import tqdm
	parser = ArgumentParser("pad a set of files into a square")
	parser.add_argument("dir", type=str, help="image directory")
	parser.add_argument("size", type=int, help="side length")
	args = parser.parse_args()
	files = os.listdir(args.dir)
	for file_name in tqdm(files):
		if file_name.endswith(".jpg"):
			image = Image.open(os.path.join(args.dir, file_name))
			new_image = pad_image(image, args.size)
			new_image.save(os.path.join(args.dir, file_name),"JPEG")
	
