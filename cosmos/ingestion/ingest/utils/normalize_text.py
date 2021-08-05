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
import ftfy
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logging.getLogger("pdfminer").setLevel(logging.WARNING)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)


def normalize_text(text: str):
    """
    Normalizes text. Standardizes usage of unicode ligatures.
    TODO: combine line-
    broken words into one token.

    :param text: Text segment to clean up
    :return: Normalized text
    """
    return ftfy.fix_text(text)



