#!/bin/sh
python3.8 -m ingest.scripts.train_postprocess \
    --logdir /logs/ --modelcfg /configs/model_config.yaml \
    --detect-weights /weights/model_weights.pth \
    --device cuda  \
    --train-img-dir       /train_dir/images \
    --train-proposals-dir /train_dir/proposals \
    --train-xml-dir       /train_dir/annotations \
    --val-img-dir       /val_dir/images \
    --val-proposals-dir /val_dir/proposals \
    --val-xml-dir       /val_dir/annotations \
    --num-processes 40 --classcfg /configs/model_config.yaml \
    --output-path /out/pp_model_weights.pth