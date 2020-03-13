"""
Tests for pdf ingestions
"""

from detect import process_doc
from typing import Mapping, TypeVar, Callable
import pytest
import json
import logging
import os

T = TypeVar('T')

# We're going to need a mock for the database call
def db_mock_fn(objs: Mapping[T, T], client: T) -> None:
    return None

def test_process_doc():
    with open(os.path.join('1.json')) as jf:
        obj = json.load(jf)
        result = process_doc(obj[0], db_mock_fn, None, config_pth='../model_config.yaml', weights_pth='../model_weights.pth')
        page = result['page_data'][0]
        assert 'detected_objs' in page
        logging.info(page['detected_objs'])
        
    








