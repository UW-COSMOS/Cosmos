from torch.utils.data import DataLoader
import torch
from torch_model.train.data_layer.xml_loader import get_colorfulness, get_radii, get_angles
from pascal_voc_writer import Writer
from os.path import join, isdir
from os import mkdir
from tqdm import tqdm
from collections import defaultdict

class InferenceHelper:
    def __init__(self, model, dataset, device):
        """
        initialize an inference object
        :param model: a MMFasterRCNN model, expected to have weights loaded
        :param dataset: an inference_loader dataset
        """
        self.model = model
        self.dataset = dataset
        self.device = device
        self.cls = [val for val in model.cls_names]

    def run(self):
        """
        run inference
        :param out: the directory to output xmls
        :return:

        """
        loader = DataLoader(self.dataset, batch_size=1, collate_fn=self.dataset.collate)
        pred_dict = defaultdict(list)
        for ex in tqdm(loader):
            batch, db_ex = ex
            windows = batch.neighbor_windows.to(self.device)
            ex = batch.center_windows.to(self.device)
            ex_color = get_colorfulness(ex).to(self.device).reshape(-1,1)
            radii = get_radii(batch.center_bbs[0],batch.neighbor_boxes[0]).to(self.device).reshape(-1,1)
            angles = get_angles(batch.center_bbs[0], batch.neighbor_boxes[0]).to(self.device).reshape(-1,1)
            windows_sub = windows[0]
            ex_sub = ex[0].unsqueeze(0)
            rois, cls_scores = self.model(ex_sub, windows_sub,radii, angles, ex_color, batch.center_bbs, self.device)
            #probabilities = torch.nn.functional.softmax(cls_scores).squeeze()
            probs, pred_idxs = torch.max(cls_scores, dim=1)
            probabilities = cls_scores.squeeze()
            bb = batch.center_bbs[0]
            pred = self.cls[pred_idxs[0]]
            # Convert the tensor to a list of coords
            pred_tuple = (bb.tolist(), pred, float(probabilities[pred_idxs[0]].item()))
            page_id = db_ex.page_id
            pred_dict[page_id].append(pred_tuple)

        return pred_dict

    def _get_predictions(self, windows, proposals):
        """
        get predictions for each img in a document
        :return: [[cls, (x1, y1, x2,y2)]]
        """
        preds = []
        rois, cls_scores = self.model(windows, proposals, self.device)
        # filter background predictions
        probs, pred_idxs = torch.max(cls_scores, dim=1)
        for i in pred_idxs:
            preds.append(self.cls[i])
        return preds



