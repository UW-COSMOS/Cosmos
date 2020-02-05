from pascal_voc_writer import Writer
import os


"""
This module takes in the ouput of model.detect from 
matterport/Mask_RCNN and ouputs the equivalent of the output
in VOC XML format On top of the model.detect output, the module
requires the class mapping that Mask_RCNN is using. Specifically,
we require:

name: the name of the file (used for output)

output_dir: the directory to output the file

size: [w,h] the size of the original image

rois: [N, (y1, x1, y2, x2)] detection bounding boxes
        class_ids: [N] int class IDs
        scores: [N] float probability scores for the class IDs
        masks: [H, W, N] instance binary masks

class_names: [c_1, c_2, ... , c_n] class name strings such that 
	if the roi list has id N, this has readable name c_N.
"""
def reorder(lst):
    """
    convert from model output to pascal voc writer output space
    lst: the list of model output bbox point
    return: reordered points for voc
    """
    return (lst[1], lst[0], lst[3], lst[2])

def model2xml(name, output_dir, size, rois, class_names, scores):
    writer = Writer(f"{name}.png", height=size[0], width=size[1])
    for ind, roi in enumerate(rois):
        # Only write scores above the provided treshold
        score = scores[ind]
        cl_name = class_names[roi[0]]
        pts = reorder(roi[1])	
        writer.addObject(cl_name, *pts, difficult=score)
    writer.save(f"{os.path.join(output_dir, name)}.xml")

