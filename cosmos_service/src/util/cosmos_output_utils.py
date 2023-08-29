from zipfile import ZipFile

from ..processing_session_types import CosmosSessionJob


def extract_file_from_job(job: CosmosSessionJob, file_path: str):
    with ZipFile(f'{job.output_dir}/{job.pdf_name}_cosmos_output.zip') as zipf:
        return zipf.open(file_path)
