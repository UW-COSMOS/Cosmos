from torch.utils.data import DataLoader
import torch
from functools import partial

def unpack_cls(cls_dict, gt_list):
    arr = map(lambda x: cls_dict[x], gt_list)
    return torch.tensor(list(arr))


def collate(item, cls_dict):
    """
    collation function for GTDataset class
    :param batch:
    :return:
    """
    return item[0][0]

class InferenceHelper:
    def __init__(self, model, dataset, device):
        """
        initialize an inference object
        :param model: a MMFasterRCNN model
        :param dataset: a dataset with or without ground truth
        """
        self.model = model
        self.dataset = dataset
        self.device = device
        self.cls = dict([(val, idx) for (idx, val) in enumerate(model.cls_names)])

    def get_predictions(self):
        """
        get predictions for each img in the dataset
        :return: [[cls, (x1, y1, x2,y2)]]
        """
        preds = []
        loader = DataLoader(self.dataset, batch_size=1, collate_fn=partial(collate, cls_dict=self.cls))
        for item in loader:
            ex = item.unsqueeze(0).float()
            rpn_cls_scores, rpn_bbox_deltas, rois, cls_preds, cls_scores, bbox_deltas = self.model(ex, self.device)
            #filter background predictions
            probs, idxs = torch.max(cls_preds, dim=2)
            mask = probs > 0.5
            npred = mask.nonzero().sum()
            print(f"made {npred} non background predictions")
            print(f"rois: {rois.shape}")

            #mask = torch.argmax(cls_preds)

