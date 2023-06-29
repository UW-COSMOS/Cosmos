import os, sys
import tempfile
sys.path.append("..")
import make_parquet as mp
from pydantic import BaseModel
from fastapi import FastAPI, UploadFile, File
from fastapi.logger import logger
from fastapi.responses import FileResponse

import shutil

app = FastAPI()

#os.environ["KEEP_INFO"] = "True"
#os.environ["JUST_PROPOSE"] = "True"
#os.environ["SKIP_AGGREGATION"]  = "True"
#os.environ["JUST_AGGREGATION"] = "True"
os.environ["CUDA_VISIBLE_DEVICES"] = "0"
os.environ['MODEL_CONFIG']="/configs/model_config.yaml"
os.environ["WEIGHTS_PTH"]="/weights/model_weights.pth"
os.environ["PP_WEIGHTS_PTH"]="/weights/pp_model_weights.pth"
os.environ["AGGREGATIONS"]="pdfs,sections,tables,figures,equations"
os.environ["LD_LIBRARY_PATH"]="/usr/local/nvidia/lib:/usr/local/nvidia/lib64"

@app.post("/process/")
def process_document(pdf: UploadFile = File(...)):
    with tempfile.TemporaryDirectory() as pdf_dir, tempfile.TemporaryDirectory() as page_info_dir, tempfile.TemporaryDirectory() as out_dir:
        try:
            with open(f"{pdf_dir}/{pdf.filename}", "wb") as f:
                shutil.copyfileobj(pdf.file, f)
        except Exception:
            return {"message": ":("}
        finally:
            pdf.file.close()

        results = mp.main_process(pdf_dir, page_info_dir, out_dir)
        mp.resize_files(out_dir)
#        mp.extract_tables(pdf_dir, out_dir)
        zip_file_name = f"{pdf.filename}_cosmos_output"
        shutil.make_archive(zip_file_name, "zip", out_dir)
        print(f"zip created at {zip_file_name}")
        print(f"{pdf_dir} for pdfs; {page_info_dir} for page info; {out_dir} for output")
        print(os.path.exists(zip_file_name + ".zip"))
        # TODO init model on startup, don't do it every time a PDF is posted
        return FileResponse(f"{zip_file_name}.zip")

@app.get("/")
def read_root():
    return {"Hello": "World"}

@app.on_event("startup")
async def startup_event():
    """
    Initialize FastAPI and add variables
    """

    import torch
    logger.info(torch.cuda.is_available())

#    # Initialize the pytorch model
#    model = Model()
#    model.load_state_dict(torch.load(
#        CONFIG['MODEL_PATH'], map_location=torch.device(CONFIG['DEVICE'])))
#    model.eval()
#
#    # add model and other preprocess tools too app state
#    app.package = {
#        "scaler": load(CONFIG['SCALAR_PATH']),  # joblib.load
#        "model": model
#    }
