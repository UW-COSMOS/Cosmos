from fastapi import APIRouter, Form, File, UploadFile, HTTPException
from util.cosmos_output_utils import *
from db.db import get_job_details
from healthcheck.annotation_metrics import *
from typing import List
import shutil
from tempfile import TemporaryDirectory

router = APIRouter(prefix="/healthcheck")

@router.post("/evaluate/{job_id}")
def evaluate_results(job_id: str, expected_bounds: List[AnnotationBounds]) -> List[DocumentAnnotationComparison]:
    """
    Evaluate the results of a recently-run COSMOS job against a list of expected region bounding boxes. For each of the 3 region label classes,
    the [average precision score](https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)#Average_precision) is calculated
    according to the following procedure:

    * For each threshold between 50% and 95%, in 5% intervals:
    * Label each Cosmos extraction as a true positive if its intersection-over-union with a ground truth region is greater than the threshold
    * Compute the [average precision](https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)#Average_precision) of the set of extractions at the given threshold
      * Average Precision is a measure of the change in precision and recall of the model as results with lower confidence scores are considered
    * Average the average precision score from each i-o-u threshold
    """
    job = get_job_details(job_id)
    comparator = AnnotationComparator(read_job_zip_file(job), expected_bounds)
    return [comparator.compare_for_label(l) for l in DEFAULT_REGION_TYPES]


@router.post("/evaluate")
def evaluate_results(
    cosmos_output: UploadFile = File(..., description="Any COSMOS output file", media_type="application/pdf"), 
    expected_bounds: str = Form(..., description="JSON object containing expected annotation bounds")) -> List[DocumentAnnotationComparison]:
    """
    Evaluate the results of any COSMOS output against a list of expected region bounding boxes. For each of the 3 region label classes,
    the [average precision score](https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)#Average_precision) is calculated
    according to the following procedure:

    * For each threshold between 50% and 95%, in 5% intervals:
    * Label each Cosmos extraction as a true positive if its intersection-over-union with a ground truth region is greater than the threshold
    * Compute the [average precision](https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)#Average_precision) of the set of extractions at the given threshold
      * Average Precision is a measure of the change in precision and recall of the model as results with lower confidence scores are considered
    * Average the average precision score from each i-o-u threshold
    """
    # FastAPI doesn't support parsing JSON directly out of multipart forms, so do it manually
    bounds = [AnnotationBounds.model_validate(b) for b in json.loads(expected_bounds)]
    if not cosmos_output.file or not cosmos_output.filename:
        raise HTTPException(status_code=400, detail="Poorly constructed form upload")
    # FastAPI upload file objects are not seekable, which is required for zip operations. Need to save to a temporary file first
    with TemporaryDirectory() as tmpdir:
        with open(f'{tmpdir}/tmp.zip', 'wb') as tmpzip:
            shutil.copyfileobj(cosmos_output.file, tmpzip)
        comparator = AnnotationComparator(ZipFile(f'{tmpdir}/tmp.zip'), bounds)
        return [comparator.compare_for_label(l) for l in DEFAULT_REGION_TYPES]

