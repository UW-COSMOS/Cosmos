Object detection 
-------------
Classifies regions (as defined by proposals) 

**NOTE GPU usage is *highly* recommended.** CPU-based detection is supported,
but untested.

## Assumptions
- PDFs have been ingested via the `ingestion` service
- `proposal_pages` collection has been created by the `proposal` service
- `DBCONNECT` is defined at runtime.
- `DEVICE` is defined at runtime

## Impact
- Reads page-level objects with proposals and `detected_objs: null` from `propose_pages` collection
- Adds `detected_objs` field containing a ranked list of classifications for the region, updating the object  
  within the `propose_pages` collection.

## Usage

```
DEVICE='cuda:0' DBCONNECT='mongodb://mongo_user:mongo_password@mongo-host:27017' docker-compose up
```
### docker-compose command
```
python3 detect.py model_config.yaml model_weights.pth 60
```

`model_config.yaml` : Path to configuration file defining the model
`model_weights.pth` : Path to the model weights file
`60` : Number of threads to use
