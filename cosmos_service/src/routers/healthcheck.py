from fastapi import APIRouter
from util.cosmos_output_utils import *
from db.db import get_job_details
from healthcheck.annotation_metrics import *
from typing import List

router = APIRouter(prefix="/healthcheck")

@router.post("/evaluate/{job_id}")
def evaluate_results(job_id: str, expected_bounds: List[AnnotationBounds]) -> List[DocumentAnnotationComparison]:
    """
    Evaluate the results of a COSMOS job against a list of expected region bounding boxes
    """
    job = get_job_details(job_id)
    comparator = AnnotationComparator(read_job_zip_file(job), expected_bounds)
    return [comparator.compare_for_label(l) for l in DEFAULT_REGION_TYPES]
