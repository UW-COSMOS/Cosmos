from mrcnn import utils
import os
from voc_utils import load_from_file
import numpy as np
from PIL import Image

class BatchDataset(utils.Dataset):

    def __init__(self, path, collapse):
        super(BatchDataset, self).__init__()
        self.path = path
        self.collapse = collapse

    def load_page(self, classes='default'):
        if classes == 'default':
            classes = ["figureRegion", "formulaRegion", "tableRegion"]
        for idx, cls in enumerate(classes):
            self.add_class("page", idx, cls)
        # now load identifiers
        images_list_f = os.path.join(self.path, self.split)
        images_list_f = '{}.txt'.format(images_list_f)
        try:
            with open(images_list_f) as fh:
                cnt = 0
                for line in fh:
                    cnt += 1
                    image_id = line.strip()
                    ipath = self.image_path(image_id)
                    im = Image.open(ipath)
                    # We're going to read the resolution from the image for now since the dimensions don't come from
                    # the annotations
                    self.add_image("page", image_id=image_id, str_id=image_id, path=ipath, width=im.size[0], height=im.size[1])
                    # We're going to read the resolution from the image for now since the dimensions don't come from
                    # the annotations
                print("loaded {} images\n".format(cnt))
        except EnvironmentError:
            print('Something went wrong with loading the file list at: {}\n'
                  'Does the file exist?'.format(images_list_f))

    def image_path(self, image_id):
        image_path = "images/{}.jpg".format(image_id)
        return os.path.join(self.path, image_path)

    def load_mask(self,image_id):
        str_id = self.image_info[image_id]["str_id"]
        anno_path = "annotations/{}.xml".format(str_id)
        anno_path = os.path.join(self.path, anno_path)
        annotation = load_from_file(anno_path)
        if self.collapse:
            annotation.collapse_classes_icdar()
        w, h = self.image_info[image_id]['width'], self.image_info[image_id]['height']
        objs = annotation.objects
        mask = np.zeros([w, h, len(objs)])
        for i, obj in enumerate(objs):
            coords = obj[1]
            mask[coords[1]:coords[3], coords[0]:coords[2], i] = 1

        clsids = np.array([self.class_names.index(obj[0]) for obj in objs])
        return mask.astype(np.bool), clsids.astype(np.int32)


