export COMET_KEY=QEQoJxtEDO2qJt1TkGMWuX0uH
export COMET_WORKSPACE=ankur-gos
export COMET_PROJECT=cosmos
export COMET_EXPERIMENT_NAME=SemanticSegmentation

python train.py \
    --data-path /data/splits \
    --save-path . \
    --gpus 1 \
    --distributed-backend dp \
    --arch deeplabv3_resnet101 \
    --epochs 90 \
    --batch-size 15 \
    --lr 0.001 \
    --momentum 0.9 \
    --wd 0.0004 \
    --pretrained

