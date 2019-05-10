from torch_model.model.model import MMFasterRCNN
from torch_model.model.utils.config_manager import ConfigManager
import torch
from torch_model.inference.inference import InferenceHelper
from torch_model.inference.data_layer.inference_loader import InferenceLoader
from ingestion.ingest_images import ImageDB



def run_inference(page_objs, model_config, weights, device_str, out_dir=None, pdf_name=None):
    """
    Main function to run inference. Writes a bunch of XMLs to out_dir
    :param page_objs: List of page objects
    :param model_config: Path to model config
    :param weights: path to weights file
    :param out_dir: Path to output directory
    :param pdf_name: Name of the pdf
    :param device_str: Device config
    """
    cfg = ConfigManager(model_config)
    model = MMFasterRCNN(cfg)
    model.load_state_dict(torch.load(weights, map_location={"cuda:0": device_str}))
    model.eval()
    def bn_train(m):
        if type(m) == torch.nn.BatchNorm2d:
            m.train()
    model.apply(bn_train)
    session, ingest_objs = ImageDB.initialize_and_ingest(page_objs,
                                                         cfg.WARPED_SIZE,
                                                         'test',
                                                         cfg.EXPANSION_DELTA,
                                                         pdf_name=pdf_name)
    loader = InferenceLoader(session, ingest_objs, cfg.CLASSES)
    device = torch.device(device_str)
    model.to(device)
    infer_session = InferenceHelper(model, loader, device)
    return infer_session.run(out_dir)

