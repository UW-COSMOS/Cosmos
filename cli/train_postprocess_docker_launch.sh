#!/bin/sh
docker run \
-it \
--name test_train_postprocess \
-e CUDA_VISIBLE_DEVICES=1 \
-v /hdd/iaross/train_dir:/train_dir \
-v /hdd/iaross/val_dir:/val_dir \
-v /hdd/iain/train_postprocess_out:/out \
iain:train_postprocess
