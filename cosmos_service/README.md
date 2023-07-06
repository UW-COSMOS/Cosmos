
To build the image and run it (from the root of the project), exposing port 8089:

```
[you@host ~/Cosmos]$ docker build -t cs -f deployment/cosmos-service.Dockerfile .
[you@host ~/Cosmos]$ docker run -p 8089:8089 cs
```

Exposes 3 endpoints:

* `POST /process` : Submit form data containing a single PDF for COSMOS processing. Begins processing in the background,
   and returns a job id that can be used to poll for progress.

* `GET /process/{job_id}/status` : Poll the process of the given processing job.

* `GET /process/{job_id}/result` : Get the results of a completed COSMOS extraction as a zip file.

Example usage:
```
URL=http://cosmos0002.chtc.wisc.edu:8089
$ curl --form 'pdf=@"/path/to/sample.pdf"' "$URL/process/"
{
  "message": "PDF Processing in Background",
  "job_id": "{job_id}",
  "status_endpoint": "/process/{job_id}/status",
  "result_endpoint": "/process/{job_id}/result"
}

# Job still in progress
$ curl "$URL/process/{job_id}/status"
{
  "job_started": true,
  "job_completed": false,
  "time_in_queue": 3.884142,
  "time_processing": 20.014362,
  "error": null
}

# Job complete
$ curl "$URL/process/{job_id}/status"
{
  "job_started": true,
  "job_completed": true,
  "time_in_queue": 3.884142,
  "time_processing": 26.038767,
  "error": null
}


# Or, job failed
$ curl "$URL/process/{job_id}/status"
{
  "job_started": true,
  "job_completed": false,
  "time_in_queue": 3.884142,
  "time_processing": 26.038767,
  "error": "Something went wrong!"
}


# Retrieve results
$ curl -o output.zip "$URL/process/{job_id}/results"
```
