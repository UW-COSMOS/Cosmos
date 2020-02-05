import falcon
from falcon import testing
import pytest

from process.app import api, ProcessPage


@pytest.fixture
def client():
    return testing.TestClient(api)

def test_process(client, monkeypatch):
    id = 1
    def get_test_img(*args, **kwargs):
        f = open('tests/test.png', 'rb')
        return {'bytes': f.read()}

    def commit_test_objs(self, page_objs, result):
        return 

    monkeypatch.setattr(ProcessPage, 'query_pageid', get_test_img)
    monkeypatch.setattr(ProcessPage, 'commit_objs', commit_test_objs)
    response = client.simulate_post('/process',
                                    params={'id': id})


