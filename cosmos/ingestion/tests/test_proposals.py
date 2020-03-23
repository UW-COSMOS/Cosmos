"""
Tests for proposal related things
"""

import pytest
from ingest.process.proposals.connected_components import get_proposals
import io
import base64
from PIL import Image


def test_num_objs():
    with open('/ingestion/tests/images/pages-43707.bytes', 'rb') as rf:
        b = rf.read()
        img = Image.open(io.BytesIO(b)).convert('RGB')
        resize_bytes = io.BytesIO()
    proposals = get_proposals(img)
    num_objs = len(proposals)
    assert num_objs < 10

    with open('/ingestion/tests/images/pages-956.bytes', 'rb') as rf:
        b = rf.read()
        img = Image.open(io.BytesIO(b)).convert('RGB')
        resize_bytes = io.BytesIO()
    proposals = get_proposals(img)
    num_objs = len(proposals)
    assert num_objs < 10


def test_no_tiny_objs():
    with open('/ingestion/tests/images/pages-43707.bytes', 'rb') as rf:
        b = rf.read()
        img = Image.open(io.BytesIO(b)).convert('RGB')
        resize_bytes = io.BytesIO()
    proposals = get_proposals(img)
    num_objs = len(proposals)
    for proposal in proposals:
        w = proposal[2] - proposal[0]
        h = proposal[3] - proposal[1]
        print(proposals)
        assert False #h >= 40 and w >= 50




