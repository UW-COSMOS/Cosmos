import pytest
import requests
import read_dir_and_request as run

def test_e2e_one_pdf():
    did, resps = run.run('/app/one_pdf', 1)
    for resp in resps:
        assert resp.status_code == 200
    resp = run.delete(did)
    assert resp.status_code == 200


def test_e2e_large_pdf():
    did, resps = run.run('/app/large_pdfs', 30)
    for resp in resps:
        assert resp.status_code == 200
    resp = run.delete(did)
    assert resp.status_code == 200


