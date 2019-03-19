from torch.utils.data import DataLoader
import torch
from pascal_voc_writer import Writer
from os.path import join, isdir
from os import mkdir
from tqdm import tqdm

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

    def run(self,out):
        """
        run inference
        :param out: the directory to output xmls
        :return:

        """
        if not isdir(out):
            mkdir(out)
        loader = DataLoader(self.dataset, batch_size=1, collate_fn=self.dataset.collate)
        xml_dict = {}
        for ex in tqdm(loader):
            batch, db_ex = ex
            page_id = db_ex.page_id
            windows = batch.neighbor_windows.to(self.device)
            ex = batch.center_windows.to(self.device)
            windows_sub = windows[0]
            ex_sub = ex[0].unsqueeze(0)
            rois, cls_scores = self.model(ex_sub, windows_sub, batch.center_bbs, self.device)
            probs, pred_idxs = torch.max(cls_scores, dim=1)
            bb = batch.center_bbs[0]
            pred = self.cls[pred_idxs[0]]
            if page_id in xml_dict:
                xml_dict[page_id].append((bb, pred))
            else:
                xml_dict[page_id]= [(bb, pred)]


        for pid in xml_dict:
            writer = Writer("", 1000,1000)
            for obj in xml_dict[pid]:
                bb, pred = obj
                x0, y0, x1, y1 = bb.long().tolist()
                writer.addObject(pred, x0, y0, x1, y1)
            writer.save(join(out, f"{pid}.xml"))

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



