from mrcnn import utils
import os
from voc_utils import load_from_file
import numpy as np

class PageDataset(utils.Dataset):
	def load_page(self, split):
		self.split = split
		path = "/experiment/pvoc_utils/{}"
		if split == "test":
			self.path = path.format("VOC_test")
		else:
			self.path = path.format("VOC")
		classes = ["figureRegion", "formulaRegion", "tableRegion"]
		for idx, cls in enumerate(classes):
			self.add_class("page", idx, cls)
		# now load identifiers 
		with open("{}/ImageSets/Main/{}.txt".format(self.path, split)) as fh:
			cnt = 0
			for line in fh:
				cnt += 1
				image_id = line.strip()
				self.add_image("page", image_id=image_id,str_id=image_id ,path=self.image_path(image_id))
			print("loaded {} images\n".format(cnt))

	def image_path(self, image_id):
		image_path = "JPEGImages/{}.jpg".format(image_id)
		return os.path.join(self.path, image_path)	
	
	def load_mask(self,image_id):
		str_id = self.image_info[image_id]["str_id"]
		anno_path = "Annotations/{}.xml".format(str_id)
		anno_path = os.path.join(self.path, anno_path)	
		annotation = load_from_file(anno_path)
		w,h = annotation.size
		objs = annotation.objects
		mask = np.zeros([w, h, len(objs)])
		class_ids = []
		for i, obj in enumerate(objs):
			coords = obj[1]
			mask[coords[1]:coords[3], coords[0]:coords[2], i] = 1

		clsids = np.array([self.class_names.index(obj[0]) for obj in objs])
		return mask.astype(np.bool), clsids.astype(np.int32)
		

