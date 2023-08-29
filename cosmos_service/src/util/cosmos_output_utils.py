from zipfile import ZipFile
from parquet_to_json import parquet_to_json
from fastapi import Request
import pandas as pd
import os
from io import BytesIO

PARQUET_COLUMN_NAMES = {
    "equations": ("equation_bb", "equation_page"),
    "figures": ("objs_bb", "objs_page"),
    "tables": ("objs_bb", "objs_page"),
}

DEFAULT_PARQUET_COLUMN_NAMES = ("bounding_box", "page_num")

def extract_file_from_job(job, file_path: str):
    with ZipFile(f'{job.output_dir}/{job.pdf_name}_cosmos_output.zip') as zipf:
        return zipf.open(file_path)


def convert_parquet_to_json(job, parquet_path: str, request: Request):
    """
    Convert a parquet file to JSON using the SKEMA bounding box sorting method, choosing columns
    to extract based on which parquet file is being parsed, then correct extracted
    image paths to match the full request URL
    """

    (bb_column, page_column) = [
        vals for key, vals in PARQUET_COLUMN_NAMES.items() if parquet_path.endswith(f"{key}.parquet")
    ] or DEFAULT_PARQUET_COLUMN_NAMES
    
    with extract_file_from_job(job, parquet_path) as parquet:
        json_data = parquet_to_json(parquet, bb_column, page_column)
    print(request.base_url)

    return json_data


