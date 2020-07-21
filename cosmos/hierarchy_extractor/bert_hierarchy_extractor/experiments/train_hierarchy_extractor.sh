python runners/train_runner.py --data-path ../data/labels.json \
  --bsz 8 --num-workers 0 --lr 1e-6 --weight-decay 0.01 \
  --warmup-updates 100 --accumulation-steps 1 --validation-interval 100 \
  --max-updates 1000 \
  --seed 1 --model-path bert-base-uncased --tag hierarchy-extractor \
  --device cuda --save-metric val_loss --save-min