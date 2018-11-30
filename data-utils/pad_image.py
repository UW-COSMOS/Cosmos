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
