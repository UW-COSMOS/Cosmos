from fastapi import UploadFile, File, Form, HTTPException, Request, APIRouter
from ..util.cosmos_output_utils import *
from ..db.db import get_job_details
from ..work_queue import queue
from ..healthcheck.annotation_metrics import *

router = APIRouter(prefix="/process")

@router.post("healthcheck/evaluate/{job_id}")
def evaluate_results(job_id: str, expected_bounds: list[AnnotationBounds]) -> List[DocumentAnnotationComparison]:
    """
    Evaluate the results of a COSMOS job against a list of expected region bounding boxes
    """
    job = get_job_details(job_id)
    comparator = AnnotationComparator(read_job_zip_file(job), expected_bounds)
    return [comparator.compare_for_label(l) for l in DEFAULT_REGION_TYPES]
