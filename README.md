# Cosmos
Interface for applying Cosmos to document segmentation

Current milestone (with demo links): https://github.com/UW-COSMOS/project-docs/tree/master/presentations_reports/milestone_3

## Services - overview
1. ingestion
    - Ingest the PDF documents
    - Separate into page-level image objects
    - Apply segmentation (separate regions of the page)
    - Apply detection (visually classify regions)
    - Postprocessing (combine regions and re-classify regions using a secondary model that uses text content)
    - Populates `pdfs`, `pages`, `page objects`
2. extraction
    - Aggregate text sections, 
    - Associate figures and tables with their respective captions
    - For table objects, extract dataframe objects
    - Populates `tables`, `object_contexts`
3. recall
    - Create an Anserini and ElasticSearch indexes on the contexts and objects
4. Visualizers, tagging, and annotation approver
    - Visual interfaces exist to 
        - Visualize and annotate the detection output (classified regions of the pages),
    - APIs exist to:
        - Access + search the Anserini/Elasticsearch indexes (`birdnest_backend`)
        - Access and annotate the page-level objects (`page_api`)
        - Get table objects in dataframe, csv, and text-preview forms

## Running the standalone images
The COSMOS architecture is service-based. Running each stage of the pipeline requires: 
    - Bringing up the worker services via `cosmos [stage] up [pdf_dir] [dataset_id]`
    - Triggering the stage via `cosmos [stage] run [pdf_dir] [dataset_id]`

A full pipeline run:
  ```
  export PDFS=./pdfs
  export DATASET_ID=first_dataset
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
Classified, identified, and extracted objects (with their contexts) can be viewed by browsing on localhost:8082


## Known issues

## Scaling
COSMOS workers are run as dask [TODO: link] workers as docker services. This
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
