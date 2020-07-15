# COVID-19 sample
As an example, a small subset of the covid-19 [xDD](https://geodeepdive.org)-derived dataset is 
available here. To process these 10 documents:


```
export COSMOS_HOME=../../cosmos
export PDFS=$(pwd)/pdfs # Path to directory of PDFs
export DATASET_ID=covid_sample # Name of dataset, for book-keeping
./cosmos all up $PDFS $DATASET_ID # prepares services of all stages
./cosmos ingest run $PDFS $DATASET_ID # start the ingestion process to ingest pdfs, separate pages, detect + classify regions 
./cosmos extract run $PDFS $DATASET_ID # start the extraction process to aggregate sections and extract table objects
./cosmos recall run $PDFS $DATASET_ID # create the searchable Anserini and ElasticSearch indexes
```
Page-level (ingestion) output can be viewed by browsing 'Page-level Extractions' on localhost:8081
Classified, identified, and extracted objects (with their contexts) can be searched by browsing on localhost:8082

See the README at the base of the repository for more information.
