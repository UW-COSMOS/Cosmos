"""
Helpers for getting PDFs ready for ingestion
"""
import uuid
import glob
import os
import base64
import pickle
import json
import subprocess
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)


def get_pdf_names(directory: str):
    """
    :param directory: Directory to process
    :param dataset_id: The dataset id. This allows you to group datasets together.
    :return: None if tmp_dir is set, else a list of pdf objects
    """
    files = glob.glob(os.path.join(directory, '*.pdf'))
    if len(files) == 0:
        raise ValueException('Empty input directory')
    return files



