# COSMOS
Platform for knowledge discovery within scientific literature.

## Overview
The COSMOS platform consists of services to ingest scientific documents, split
the documents into logical regions, analyze and classify those regions (as
'body text', 'tables', 'figures', 'references', 'section headers', etc.),
combine and contextualize related regions, and provide an interface for
searching for high-quality, contextualized extractions.

COSMOS is a suite of [Docker](https://www.docker.com/) services that
work together to process the documents.  Broadly speaking, they are grouped
into the following categories:

1. Ingestion
    - Ingest the PDF documents
    - Separate into page-level image objects
    - Apply segmentation (separate regions of the page)
    - Apply detection (visually classify regions into **objects**)
    - Apply OCR ([Tesseract](https://github.com/tesseract-ocr/tesseract) via [pytesseract](https://pypi.org/project/pytesseract/))
    - Postprocessing (combine and re-classify regions using a pytorch model that leverages text content)
    - Populates `pdfs`, `pages`, `page objects` tables
2. Extraction
    - Aggregate text sections 
    - Associate figures and tables with their respective captions
    - For table objects, extract dataframe objects
    - Populates `tables`, `object_contexts` tables
3. Recall
    - Create an [Anserini](https://github.com/castorini/anserini) and [ElasticSearch](https://www.elastic.co/) indexes on the contexts and objects
4. Visualizers, tagging, and annotation approver
    - Visualize and annotate the detection output (classified regions of the pages)
    - Provide initial annotations (tags) for a document for training purposes
5. APIs
    - Access + search the [Anserini](https://github.com/castorini/anserini) and [Elasticsearch](https://www.elastic.co/) indexes (`birdnest_backend`)
    - Access and annotate the page-level objects (`page_api`)
    - Get table objects in dataframe, csv, and text-preview forms

Data is persisted in a [mysql](https://www.mysql.com/) backend.

## Requirements
COSMOS requires both the Docker engine and Docker Compose to be installed. See the guides
for each to install on your system:

  - [Docker Engine](https://docs.docker.com/engine/install/#server)
  - [Docker Compose](https://docs.docker.com/compose/install/)

Application of the detection model is memory intensive and either requires usage of a GPU (see [GPUs](#GPUs) below)
or allocating significant memory to docker processes ([Docker resource usage](https://docs.docker.com/config/containers/resource_constraints/)). At least 12GB should be allocated when running in CPU mode.

Canonical docker images are provided and designated within the `docker-compose` commands invoked below. 

By default, subdirectories are created in the runtime directory to hold the persistent database, indexes, and and tmp scratch space needed within the running images. These paths
(and the database credentials) can be modified within the `cosmos/cosmos` script

## Running the standalone images
The COSMOS architecture is service-based, and canonical images are provided. Running each stage of the pipeline requires: 

  - Bringing up the worker services via `cosmos [stage] up [pdf_dir] [dataset_id]`
  - Running the process via `cosmos [stage] run [pdf_dir] [dataset_id]`

The stage names are `ingest`, `extract`, and `recall`.

A full pipeline run (from the `./cosmos/cosmos` directory):
  ```
  export PDFS=./pdfs # Path to directory of PDFs
  export DATASET_ID=first_dataset # Name of dataset, for book-keeping
  ./cosmos all up $PDFS $DATASET_ID # prepares services of all stages
  ./cosmos ingest run $PDFS $DATASET_ID # start the ingestion process to ingest pdfs, separate pages, detect + classify regions 
  ./cosmos extract run $PDFS $DATASET_ID # start the extraction process to aggregate sections and extract table objects
  ./cosmos recall run $PDFS $DATASET_ID # create the searchable Anserini and ElasticSearch indexes
  ```

The stages must be run in order. `recall` requires the `object_contexts`, which
are created in `extract`. `extract` requires the `page_objects`, which are
created in `ingest`.

**Important** Note that the `run` commands submit PDFs, pages, etc into a work
queue. If submitting a late-stage run does not produce expected output, it is
possible that the earlier stage(s) have not finished running.

Page-level (ingestion) output can be viewed by browsing 'Page-level Extractions' on localhost:8081
Classified, identified, and extracted objects (with their contexts) can be searched by browsing on localhost:8082

## Scaling
COSMOS workers are run as [Dask](https://dask.org/) worknodes as docker services. This
allows services to be scaled arbitrarily, by adding a new node to the docker
swarm and then scaling the service:

```
# on master node
docker swarm join-token worker

# on worker node, copy/ paste the join command output from the master:
docker swarm join --token SWMTKN-1-xxyyzz 128.xxx.yyy.zzz:port

# back on master node, scale up the desired service:
docker service scale cosmos-worker-2=2

```

## GPUs
**GPU usage is highly recommended for the detection process**
Currently, GPUs are not required, but performance without them is not
guaranteed. It requires at least 12GB to be allocated to the docker daemon. To
enable GPUs, ensure that the worker2 service definition in `docker-compose.yml`
includes cuda:0 as the device:
```
- DEVICE=cuda:0
- CUDA_VISIBLE_DEVICES=0
```

For CPU-based detection, make sure the 'cpu' device is uncommented in `docker-compose.yml`:

```
- DEVICE=cpu
- CUDA_VISIBLE_DEVICES=''
```

## Schema
### pdfs
Data about the PDF documents ingested

| Column | Type | Description
|--------|------|------------
id | int(11) Auto Increment | Sequential ID
dataset_id |	varchar(200) NULL | Name of dataset
pdf_name | varchar(200) NULL | Filename of the PDF
name | varchar(200) NULL | UUID assigned to PDF
meta | json NULL | PDFMiner-derived unicode representation of the PDF
meta_dimension |json NULL | Page size of metadata representation

### pages
Page-level information (including image bytes)

| Column | Type | Description
|--------|------|------------
| id	   | int(11) Auto Increment | Sequential ID
|pdf_id	 | int(11) NULL | FOREIGN key to pdfs.id
|page_width | 	int(11) NULL | Size of page-level image
|page_height|	int(11) NULL | Size of page-level image
| page_number | 	int(11) NULL | Page number within PDF
| bytes	| longblob NULL | Byte form of page-level image (required for annotation viewer)

### page_objects
Objects classified on the pages via detection process

| Column | Type | Description
|--------|------|------------
|id |	int(11) Auto Increment|  Sequential ID
|page_id |	int(11) NULL| FOREIGN key to pages.id
|context_id	|int(11) NULL| FOREIGN key to object_contexts.id
|bytes | longblob NULL| Image of extracted object (bytes)
|content|	varchar(10000) NULL| OCRed text within extracted objects
|bounding_box|	json NULL| Coordinates of object on page
|init_cls_confidences|	json NULL| Class + confidence levels from initial detection
|cls	|varchar(200) NULL| Class, after model-based post-processing
|pp_rule_cls|	varchar(200) NULL| Class, after rule-based post-processing
|annotated_cls	| varchar(200) NULL| Annotated (manually set) class
|confidence|	decimal(9,6) NULL| Confidence of post-processing class assignment
|classification_success|	tinyint(1) NULL| Annotation -- true/false flag of classification success
|proposal_success|	tinyint(1) NULL| Annotation -- true/false flag of region proposal success
|summary|	text NULL| gensim-derived summary of text content
|keywords|	text NULL| gensim-derived keywords of text content

### object_contexts
Objects, aggregated into related contexts (e.g. body text combined into sections or tables grouped with their caption)

| Column | Type | Description
|--------|------|------------
|id |	int(11) Auto Increment| Sequential ID
|pdf_id |	int(11) NULL| FOREIGN key to pdfs.id
|cls |	varchar(200) NULL| Class of context (text section, figure+caption, table+caption, or equation)
|header_id |	int(11) NULL| FOREIGN key to pages_objects.id -- ID of the parent/header object within the context
|header_content |	text NULL| OCR Text of header object
|content |	longtext NULL| Combined OCR text of all objects within context
|summary|	text NULL| gensim-derived summary of text content
|keywords|	text NULL| gensim-derived keywords of text content

### tables
Tables, as recognized via detection, converted into pandas dataframes

| Column | Type | Description
|--------|------|------------
|id	| int(11) Auto Increment | Sequential ID
|page_object_id| 	int(11) NULL | FOREIGN key to page_objects.id
|page_id| 	int(11) NULL | FOREIGN key to pages.id
|df| 	longblob NULL | pandas dataframe for table object


## Services
A list of the current services, their roles, and interactions

- `cosmos_adminer` - web interface to mysql database, running on localhost:8080. Username/passwords are set in `./cosmos` script
- `cosmos_agg_worker1` - DASK worker for section aggregation
- `cosmos_birdnest_backend` - Search API for recall. Simple interface to Anserini + Elasticsearch databases
- `cosmos_create_schema` - Initial schema creation
- `cosmos_dbwebapp` - Simple helper service to echo database availability
- `cosmos_es01` - Elasticsearch node for recall
- `cosmos_gateway` - Visualization frontend for page-level detection visualization 
- `cosmos_ingestion` - PDF ingestion service
- `cosmos_kbviz` - Visualization frontend for Anserini + Elasticsearch search interface
- `cosmos_mysql-server-1` - central database
- `cosmos_scheduler` - DASK scheduler. Jobs are submitted to here and then passed along to various worker nodes
- `cosmos_search_backend` - Page-level API
- `cosmos_table_worker1` - Worker to extract dataframe objects from tables
- `cosmos_worker1` - Worker for non-GPU tasks within ingestion stage
- `cosmos_worker2` - Worker for GPU tasks within ingestion stage

# License and Acknowledgements
All development work supported by DAPRA ASKE HR00111990013 and UW-Madison.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this repo except in compliance with the License.
You may obtain a copy of the License at:

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
