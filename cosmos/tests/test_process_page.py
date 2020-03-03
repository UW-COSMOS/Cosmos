import pytest
import io
import requests
from PIL import Image

def get_json():
    img = Image.open('test.png')
    resize_bytes_stream = io.BytesIO()
    img.save(resize_bytes_stream, format='PNG')
    resize_bytes_stream.seek(0)
    resize_bytes = resize_bytes_stream.read()
    resize_bytes = base64.b64encode(resize_bytes).decode('ASCII')
    {'bytes': resize_bytes}


def test_process_page():
    js = get_json()
    result = requests.post('http://process_pages:8001/process', json=js, timeout=60)
    assert result.status_code == 200
    




