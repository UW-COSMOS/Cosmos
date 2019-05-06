"""
Tests for pdf ingestions
"""

from src.pdf_ingestion import run_pdf_ingestion, run_ghostscript
from typing import Mapping, TypeVar, Callable
import pytest

T = TypeVar('T')

# We're going to need a mock for the database call
def db_mock_fn(objs: Mapping[T, T]) -> None:
    return None

def test_pdf_ingestion():
    pdfs = run_pdf_ingestion(db_mock_fn, run_ghostscript)
    print(pdfs)








