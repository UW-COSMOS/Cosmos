import tempfile
import os
import shutil
from fastapi import UploadFile, File, Form, HTTPException, Request, APIRouter
from fastapi.responses import FileResponse, Response
import uuid
from util.cosmos_output_utils import *
from model.models import JobCreationResponse, JobStatus, CosmosJSONTextResponse, CosmosJSONImageResponse, ExtractionType
from db.db import SessionLocal, CosmosSessionJob, get_cached_job_for_pdf, get_job_details
from work_queue import queue

router = APIRouter(prefix="/process")

def _build_process_response(message, job_id, request_url):
    """Return the ID of a created job alongside the URLs that a client can use to query that job's status"""
    return JobCreationResponse(
        message=message,
        job_id=job_id,
        status_endpoint=replace_url_suffix(request_url, f"{job_id}/status"),
        result_endpoint=replace_url_suffix(request_url, f"{job_id}/result")
    )

def _save_request_pdf(job_id: str, pdf: UploadFile):
    """Make a non-temporary directory to store the request PDF and job output.
    Must be cleaned up in a separate job due to asynchronous processing and retrieval
    """
    job_output_dir = f"{tempfile.gettempdir()}/{job_id}"
    os.mkdir(job_output_dir)
    try:
        with open(f"{job_output_dir}/{pdf.filename}", "wb") as f:
            shutil.copyfileobj(pdf.file, f)
    except Exception:
        raise HTTPException("Unable to save PDF for processing")
    finally:
        pdf.file.close()
    
    return job_output_dir

@router.post("/", status_code=202)
async def process_document( 
    request: Request, 
    pdf: UploadFile = File(..., description="The document to process with COSMOS", media_type="application/pdf"), 
    compress_images: bool = Form(True, description="Whether to generate compressed or full-resolution images of extractions"), 
    use_cache: bool = Form(True, description="Whether to reuse cached results for the given PDF, if present")
    ) -> JobCreationResponse:
    """
    Accept a new PDF document for COSMOS processing. Saves the PDF to disk, 
    then adds it to a queue for subsequent processing
    """
    if not pdf.file or not pdf.filename:
        raise HTTPException(status_code=400, detail="Poorly constructed form upload")

    # Check for whether a copy of the cached PDF already exists
    pdf_hash, pdf_len, existing_job_id = get_cached_job_for_pdf(pdf.file)
    if use_cache and existing_job_id is not None:
        return _build_process_response("Existing PDF Processing Job Found", existing_job_id, request.url)

    job_id = str(uuid.uuid4())

    job_output_dir = _save_request_pdf(job_id, pdf)

    # populate the job in its default state (not started)
    with SessionLocal() as session:
        session.add(CosmosSessionJob(job_id, pdf.filename.replace('.pdf', ''), pdf_hash, pdf_len, job_output_dir))
        session.commit()

    await queue.put((job_output_dir, job_id, compress_images))

    return _build_process_response("PDF Processing in Background", job_id, request.url)

@router.get("/{job_id}/status")
def get_processing_status(job_id: str) -> JobStatus:
    """
    Return the current status of a given pdf in the COSMOS processing queue. If `job.is_completed`,
    then the results of the processing can be retrieved from `/process/{job_id}/result`
    """
    with SessionLocal() as session:
        job : CosmosSessionJob = session.get(CosmosSessionJob, job_id)
        if not job:
            raise HTTPException(status_code=404, detail="Job not found")
        
        return JobStatus(
            job_started=job.is_started,
            job_completed=job.is_completed,
            time_in_queue=job.time_in_queue,
            time_processing=job.time_processing,
            error=job.error
        )

@router.get("/{job_id}/result")
def get_processing_result(job_id: str) -> FileResponse:
    """
    Return the zip file containing the results of a completed COSMOS pipeline. Return status 400 if the 
    job is in a not-complete state
    """
    job = get_job_details(job_id)
    output_file = f"{job.pdf_name}_cosmos_output.zip"
    return FileResponse(f"{job.output_dir}/{output_file}", filename=output_file)

@router.get("/{job_id}/result/text")
def get_processing_result_text_segments(job_id: str, request: Request) -> List[CosmosJSONTextResponse]:
    """
    Return the text segments extracted by COSMOS and their bounding boxes as a list of JSON objects
    """
    job = get_job_details(job_id)
    response_json = convert_parquet_to_json(job, f'{job.pdf_name}.parquet', request)
    return [ CosmosJSONTextResponse(**p) for p in response_json ]

@router.get("/{job_id}/result/text-layer")
def get_processing_result_text_segments(job_id: str, request: Request) -> List[CosmosJSONTextResponse]:
    """
    Return every text segment in the PDF and their bounding boxes as a list of JSON objects
    """
    job = get_job_details(job_id)
    return convert_full_text_layer_to_json(job)

@router.get("/{job_id}/result/extractions/{extraction_type}")
def get_processing_result_extraction(job_id: str, extraction_type: ExtractionType, request: Request) -> List[CosmosJSONImageResponse]:
    """
    Return COSMOS figure/table/equation extractions and their bounding boxes as a list of JSON objects, 
    as well as links to their images
    """
    job = get_job_details(job_id)
    response_json = convert_parquet_to_json(job, f'{job.pdf_name}_{extraction_type.value}.parquet', request)
    return [ CosmosJSONImageResponse(**p) for p in response_json ]


@router.get("/{job_id}/result/images/{image_path}")
def get_processing_result_image(job_id: str, image_path: str) -> Response:
    """
    Extract a single image from the zip output of the given job and return it with the appropriate mimetype
    """
    job = get_job_details(job_id)
    mime_type = 'image/png' if image_path.endswith('.png') else 'image/jpeg'
    with extract_file_from_job(job, image_path) as image:
        return Response(content=image.read(), media_type=mime_type)
