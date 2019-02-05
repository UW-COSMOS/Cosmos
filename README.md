# Mask-RCNN-exp
Interface for applying Mask-RCNN to document segmentation

# Running the model

1. Switch to the exp directory
2. ```
docker-compose build
```
3. ```
docker-compose run test bash
```
4a. To run with CPU (untested, might need to finagle with docker image):
```
CUDA_VISIBLE_DEVICES="" python run.py [PDF_DIR] -w [PATH_TO_WEIGHTS_FILE] -t [NUM_THREADS]
```

4b. To run with GPU:

```
python run.py [PDF_DIR] -w [PATH_TO_WEIGHTS_FILE] -t [NUM_THREADS]
```

