# Mask-RCNN-exp
Interface for applying Mask-RCNN to document segmentation

# Running the model

1. Switch to the exp directory
2. Build the docker image
```
docker-compose build
```
3. Create a docker bash env
```
docker-compose run test bash
```

4. Start postgres
```
service postgresql start
```

5. Create the cosmos database
```
psql -U postgres

psql (9.5.14)
Type "help" for help.

postgres=# create database cosmos;
```
4a. Now run (Currently set to CPU mode)
```
python run.py [PDF_DIR] -w [PATH_TO_WEIGHTS_FILE] -t [NUM_THREADS]
```


