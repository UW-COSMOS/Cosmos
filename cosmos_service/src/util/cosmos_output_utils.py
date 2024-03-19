"""
Collection of miscellaneous utilities for working with the zipfile output of a COSMOS job.
Used to serve various /results endpoints
"""

from zipfile import ZipFile
from .parquet_to_json import parquet_to_json
from .text_layer_utils import *
from fastapi import Request
import json
from os import path
from typing import List
import re

PARQUET_COLUMN_NAMES = {
    "equations": ("equation_bb", "equation_page", ["content"]),
    "figures": ("obj_bbs", "obj_page", []),
    "tables": ("obj_bbs", "obj_page", []),
}

PARQUET_SUFFIXES = ['', *[f'_{suffix}' for suffix in PARQUET_COLUMN_NAMES.keys()]]

DEFAULT_PARQUET_COLUMN_NAMES = ("bounding_box", "page_num", ["img_pth"])


def read_job_zip_file(job):
    """ Read the output zip file of a job """
    return ZipFile(f'{job.output_dir}/{job.pdf_name}_cosmos_output.zip')

def extract_file_from_job(job, file_path: str):
    """ Extract the specified file from the specified completed job """
    with read_job_zip_file(job) as zipf:
        return zipf.open(file_path, 'r')

def replace_url_suffix(request_url, suffix):
    """Replace the given request_url after /process/ with the given suffix"""
    return re.sub("/process/.*", f"/process/{suffix}", f"{request_url}")

def _update_json_entry(json_entry: dict, request_path:str , bb_column: str, page_column: str, exclude: List[str]):
    """
    Re-map the img_pth field in parquet output from the local filesystem path to a full URL,
    and clear out any specified fields that should be excluded from the final output
    """
    for e in exclude:
        json_entry.pop(e)
    if "img_pth" in json_entry:
        json_entry["img_pth"] = path.join(request_path, path.split(json_entry["img_pth"])[1])

    # Each parquet file has a different column name for bounding box and page, standardize them
    # Layoutparser gives BBs as floats while Cosmos gives ints, standardize to int here
    json_entry["bounding_box"] = [int(bb) for bb in json_entry.pop(bb_column)]
    json_entry["page_num"] = json_entry.pop(page_column)

    return json_entry

def _get_parquet_read_parameters(parquet_path: str, pdf_name: str):
    """
    Helper function to get the relevant names of columns to include and exclude from an output
    parquet file. Each output file contains slightly different column names.
    """
    for key, values in PARQUET_COLUMN_NAMES.items():
        print(f'Checking *{key}.parquet and {path.basename(parquet_path)} against {pdf_name}.parquet')
        if parquet_path.endswith(f"{key}.parquet") and path.basename(parquet_path) != f"{pdf_name}.parquet":
            return values

    return DEFAULT_PARQUET_COLUMN_NAMES

def convert_parquet_to_json(job, parquet_path: str, request: Request):
    """
    Convert a parquet file to JSON using the SKEMA bounding box sorting method, choosing columns
    to extract based on which parquet file is being parsed, then correct extracted
    image paths to match the full request URL
    """

    image_base_url = replace_url_suffix(request.url, f"{job.id}/result/images")
    (bb_column, page_column, exclude) = _get_parquet_read_parameters(parquet_path, job.pdf_name)

    with extract_file_from_job(job, parquet_path) as parquet:
        json_data = parquet_to_json(parquet, bb_column, page_column) or []

    return [_update_json_entry(e, image_base_url, bb_column, page_column, exclude) for e in json_data]


def convert_parquet_to_json_file(parquet_path: str, pdf_name: str):
    """
    Convert a parquet file to JSON using the SKEMA bounding box method, prior to
    archiving it as a zip file
    """

    (bb_column, page_column, exclude) = _get_parquet_read_parameters(parquet_path, pdf_name)
    json_data = parquet_to_json(parquet_path, bb_column, page_column) or []
    updated_data = [_update_json_entry(e, '', bb_column, page_column, exclude) for e in json_data]
    with open(parquet_path.replace('.parquet','.json'), 'w') as output_json:
        output_json.write(json.dumps(updated_data, indent=2))

def convert_full_text_layer_to_json(job) -> List[str]:
    """ Get the full text layer of the input PDF, regardless of COSMOS labelling. PyMuPDF provides
        the `gettext` command line utility to arrange the text layer of a PDF in reading-order.
    """
    pdf_path = f"{job.output_dir}/{job.pdf_name}.pdf"
    return PyMuPDFGetTextWrapper(pdf_path).text.split('\f')

def extract_urls_from_text_layer(job) -> List[str]:
    """ """
    pdf_path = f"{job.output_dir}/{job.pdf_name}.pdf"
    return PyMuPDFGetTextWrapper(pdf_path).search_text([GITHUB_RE, ANY_SITE_RE])
