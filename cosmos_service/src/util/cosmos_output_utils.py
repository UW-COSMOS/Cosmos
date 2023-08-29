"""
Collection of miscellaneous utilities for working with the zipfile output of a COSMOS job.
Used to serve various /results endpoints
"""

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
    """
    Extract the specified file from the specified completed job
    """
    with ZipFile(f'{job.output_dir}/{job.pdf_name}_cosmos_output.zip') as zipf:
        return zipf.open(file_path, 'r')

def _update_json_entry(json_entry: dict, request_path:str , exclude: List[str]):
    """
    Re-map the img_pth field in parquet output from the local filesystem path to a full URL,
    and clear out any specified fields that should be excluded from the final output
    """
    for e in exclude:
        json_entry.pop(e)
    json_entry["img_pth"] = path.join(request_path, path.split(json_entry["img_pth"])[1])
    
    return json_entry

def _get_parquet_read_parameters(parquet_path: str):
    """
    Helper function to get the relevant names of columns to include and exclude from an output
    parquet file. Each output file contains slightly different column names.
    """
    for key, values in PARQUET_COLUMN_NAMES.items():
        if parquet_path.endswith(f"{key}.parquet"):
            return values
    
    return DEFAULT_PARQUET_COLUMN_NAMES

def convert_parquet_to_json(job, parquet_path: str, request: Request):
    """
    Convert a parquet file to JSON using the SKEMA bounding box sorting method, choosing columns
    to extract based on which parquet file is being parsed, then correct extracted
    image paths to match the full request URL
    """
    
    image_base_url = re.sub("/process/.*", f"/process/{job.id}/result/images", f"{request.url}")
    (bb_column, page_column, exclude) = _get_parquet_read_parameters(parquet_path)
    
    with extract_file_from_job(job, parquet_path) as parquet:
        json_data = parquet_to_json(parquet, bb_column, page_column)
    print(request.base_url)

    return [_update_json_entry(e, image_base_url, exclude) for e in json_data]


