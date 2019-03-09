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
        loader = DataLoader(self.dataset, batch_size=1, collate_fn=self.dataset.collate, )
        for doc in tqdm(loader):
            windows, proposals, identifier = doc
            proposals.change_format("xyxy")
            windows = windows.to(self.device)
            preds = self._get_predictions(windows)
            writer = Writer("", 1000,1000)
            for i in range(len(preds)):
                pred = preds[i]
                x0, y0, x1, y1 = proposals[i, :].long().tolist()
                writer.addObject(pred, x0, y0, x1, y1)
            writer.save(join(out, f"{identifier}.xml"))
    def _get_predictions(self, windows):
        """
        get predictions for each img in a document
        :return: [[cls, (x1, y1, x2,y2)]]
        """
        preds = []
        rois, cls_scores = self.model(windows, self.device)
        # filter background predictions
        probs, pred_idxs = torch.max(cls_scores, dim=1)
        for i in pred_idxs:
            preds.append(self.cls[i])
        return preds



