Region Proposals
-------------
Proposes rectangular regions of a page.

## Assumptions
- PDFs have been ingested via the `ingestion` service
- `DBCONNECT` is defined at runtime.

## Impact
- Reads page-level objects (with `proposal: null`) from `pages` collection
- Adds `proposals` field containing a list of regions, writes to
  `propose_pages` collection.
- Writes a `{proposal: true}` flag to the page object in the `pages`
  collection. 

## Usage

```
DBCONNECT='mongodb://mongo_user:mongo_password@mongo-host:27017' docker-compose up
```
### docker-compose command
```
python3 propose.py 60
```

`60` : Number of threads to use
`--clean` : if included, then delete the page-level `bytes` and `resize_bytes`
objects from the `pages` collection to save space
