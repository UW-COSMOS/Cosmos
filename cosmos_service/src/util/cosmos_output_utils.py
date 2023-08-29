from zipfile import ZipFile
from .parquet_to_json import parquet_to_json
from fastapi import Request
import pandas as pd
from os import path
from typing import List
from io import BytesIO
import re

PARQUET_COLUMN_NAMES = {
    "equations": ("equation_bb", "equation_page", ["content"]),
    "figures": ("objs_bb", "objs_page", ["content"]),
    "tables": ("objs_bb", "objs_page", ["content"]),
}

DEFAULT_PARQUET_COLUMN_NAMES = ("bounding_box", "page_num", [])

def extract_file_from_job(job, file_path: str):
    with ZipFile(f'{job.output_dir}/{job.pdf_name}_cosmos_output.zip') as zipf:
        return zipf.open(file_path, 'rb')



def _update_json_entry(json_entry: dict, request_path:str , exclude: List[str]):
    for e in exclude:
        json_entry.pop(e)
    json_entry["img_pth"] = path.join(request_path, path.split(json_entry["img_pth"])[1])
    
    return json_entry

def convert_parquet_to_json(job, parquet_path: str, request: Request):
    """
    Convert a parquet file to JSON using the SKEMA bounding box sorting method, choosing columns
    to extract based on which parquet file is being parsed, then correct extracted
    image paths to match the full request URL
    """

    image_base_url = re.sub("/process/.*", f"/process/{job.id}/result/image", f"{request.url}")

    (bb_column, page_column, exclude) = ([
        vals for key, vals in PARQUET_COLUMN_NAMES.items() if parquet_path.endswith(f"{key}.parquet")
    ] or [DEFAULT_PARQUET_COLUMN_NAMES])[0]
    
    with extract_file_from_job(job, parquet_path) as parquet:
        json_data = parquet_to_json(parquet, bb_column, page_column)
    print(request.base_url)

    return [_update_json_entry(e, image_base_url, exclude) for e in json_data]


