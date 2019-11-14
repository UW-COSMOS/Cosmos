PDF Ingestion
-------------
Ingests target PDFs into the mongodb backend and splits into page-level objects.

**NOTE** Current implementation ignores PDFs larger than the maximum mongodb document size of 16MB (see https://github.com/UW-COSMOS/Cosmos/issues/54) 

## Assumptions
- `INPUT_DIR` and `DBCONNECT` are defined at runtime.
- PDFs are stored directly under the `INPUT_DIR` (they are found via globbing `INPUT_DIR/*.pdf`)

## Impact
- `INPUT_DIR` is mounted as /input into running docker image
- Writes PDF bytes + identifying information to `raw_pdfs` collection
- Runs ghostscript to split the PDF into pages, store bytes + identifying information to `pages` collection

## Usage

```
INPUT_DIR=/path/to/pdfs DBCONNECT='mongodb://mongo_user:mongo_password@mongo-host:27017' docker-compose up
```

### docker-compose command
```
python3 pdf_ingestion.py /input 60 --skip
```

`/input` : PDF directory
`60` : Number of threads to use
`--skip` : If true, skip already-ingested PDFs (matched by filename)
