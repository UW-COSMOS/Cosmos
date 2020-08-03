python -m ingest.scripts.train_detection --hyperopt-config cosmos/config/hyperopt_config.yaml \
      --max-evals 1 --train-dir contracts/train/ \
      --val-dir contracts/val/ --weights-dir cosmos/weights/ \
      --pretrain-weights cosmos/weights/model_weights.pth \
      --model-config cosmos/config/model_config.yaml \
      --train-config cosmos/config/train_config.yaml