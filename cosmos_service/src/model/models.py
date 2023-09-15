from pydantic import BaseModel, Field, HttpUrl
from pydantic.schema import Optional
from typing import Tuple
from enum import Enum

# Response Models

class JobCreationResponse(BaseModel):
    message: str = Field(..., description="Message indicating whether a job was created or retrieved from the cache successfully")
    job_id: str = Field(..., description="ID of the created or retrieved job")
    status_endpoint: HttpUrl = Field(..., description="Endpoint for polling the status of an in-progress job")
    result_endpoint: HttpUrl = Field(..., description="Endpoint for retrieving the results of a completed job")

class JobStatus(BaseModel):
    job_started: bool = Field(..., description="Whether the job has started running on a GPU")
    job_completed: bool = Field(..., description="Whether the job has competed successfully")
    error: Optional[str] = Field(..., description="Whether the job has failed with an error", nullable=True)
    time_in_queue: float = Field(...,description="Time the job was queued before running")
    time_processing: Optional[float] = Field(...,description="Time the job has spent running on a GPU")

class CosmosJSONBaseResponse(BaseModel):
    pdf_name: Optional[str] = Field(description="Name of the PDF the item was extracted from")
    page_num: Optional[str] = Field(description="Page in the PDF the item was extracted from")
    bounding_box: Optional[Tuple[int, int, int, int]] = Field(description="Coordinates of the extracted item on its page")
    detect_score: Optional[float] = Field(description="Initial confidence in the label given to the extracted item")
    content: Optional[str] = Field(description="The extracted text")
    postprocess_score: Optional[float] = Field(description="Confidence in the label given to the extracted item after post-processing")


class CosmosJSONImageResponse(CosmosJSONBaseResponse):
    img_pth: Optional[HttpUrl] = Field(description="Temporary URL from which the extracted image can be retrieved")

class CosmosJSONTextResponse(CosmosJSONBaseResponse):
    detect_cls: Optional[str] = Field(description="Initial label given to the extracted item")
    postprocess_cls: Optional[str] = Field(description="Label given to the extracted item after post-processing")

# Request Models

class ExtractionType(Enum):
    Equations = "equations"
    Tables = "tables"
    Figures = "figures"
