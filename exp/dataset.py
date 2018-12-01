from mrcnn import utils
import os
from voc_utils import load_from_file
import numpy as np

class PageDataset(utils.Dataset):

    def __init__(self, split, path):
        super()
        self.split = split
        self.path = path

    def load_page(self, test_dir='VOC_test', train_dir='VOC', classes='default'):
        if classes == 'default':
            classes = ["figureRegion", "formulaRegion", "tableRegion"]
        if self.split == "test":
            self.path = self.path.format(test_dir)
        else:
            self.path = self.path.format(train_dir)
        for idx, cls in enumerate(classes):
            self.add_class("page", idx, cls)
        # now load identifiers
        with open("{}/ImageSets/Main/{}.txt".format(self.path, self.split)) as fh:
            cnt = 0
            for line in fh:
                cnt += 1
                image_id = line.strip()
                self.add_image("page", image_id=image_id, str_id=image_id, path=self.image_path(image_id))
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


