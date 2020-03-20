from ingest.process.detection.src.torch_model.model.model import MMFasterRCNN
from ingest.process.detection.src.torch_model.model.utils.config_manager import ConfigManager
import torch
from ingest.process.detection.src.torch_model.inference.inference import InferenceHelper
from ingest.process.detection.src.torch_model.inference.data_layer.inference_loader import InferenceLoader
from ingest.process.detection.src.utils.ingest_images import ImageDB


def get_model(model_config, weights, device_str):
    cfg = ConfigManager(model_config)
    model = MMFasterRCNN(cfg)
    model.load_state_dict(torch.load(weights, map_location=device_str))
    model.eval()
    def bn_train(m):
        if type(m) == torch.nn.BatchNorm2d:
            m.train()
    model.apply(bn_train)
    device = torch.device(device_str)
    model.to(device)
    return model

def run_inference(model, page_objs, model_config, device_str, session):
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
    ingest_objs = ImageDB.initialize_and_ingest(page_objs,
                                                         cfg.WARPED_SIZE,
                                                         'test',
                                                         cfg.EXPANSION_DELTA,
                                                         session)
    loader = InferenceLoader(ingest_objs, cfg.CLASSES, session)
    device = torch.device(device_str)
    infer_session = InferenceHelper(model, loader, device)
    print("Running infer in session")
    results, softmax_results = infer_session.run()
    ImageDB.cleanup(ingest_objs, session)
    print("Returning results")
    return results, softmax_results

