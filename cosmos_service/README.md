
To build the image and run it, exposing port 8089:

``
docker build -t cs .; docker run -p 8089:8089 cs
```


Example usage:
```
curl --location 'http://cosmos0002.chtc.wisc.edu:8089/process/' \
--form 'pdf=@"/Users/iaross/Downloads/johnshopkinsuniversityappliedphysicslaboratoryBuckyModel.pdf"' \
--output bucky_cosmos_output.zip
```
