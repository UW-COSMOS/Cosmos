#!/bin/bash
MAX_UPDATES=10000
WARMUP_UPDATES=150
LR=1e-03
MAX_SENTENCES=16
SEED=1
DATA_DIR=/data
#ROBERTA_PATH=$DATA_DIR/model.pt
ROBERTA_PATH=/data/model.pt

USER_DIR=/kbc

CUDA_VISIBLE_DEVICES=0 fairseq-train --fp16 \
    $DATA_DIR \
    --user-dir $USER_DIR \
    --restore-file $ROBERTA_PATH \
    --reset-optimizer --reset-dataloader --reset-meters \
    --no-save-optimizer-state \
    --best-checkpoint-metric accuracy --maximize-best-checkpoint-metric \
    --task kbc --init-token 0 --bpe gpt2 \
    --arch roberta_base --max-positions 512 \
    --dropout 0.1 --attention-dropout 0.1 --weight-decay 0.01 \
    --criterion kbc_criterion \
    --optimizer adam --adam-betas '(0.9, 0.98)' --adam-eps 1e-06 --clip-norm 0.0 \
    --lr-scheduler polynomial_decay --lr $LR \
    --warmup-updates $WARMUP_UPDATES --total-num-update $MAX_UPDATES \
    --max-sentences $MAX_SENTENCES \
    --max-update $MAX_UPDATES \
    --log-format simple --log-interval 25 \
    --seed $SEED \
    --skip-invalid-size-inputs-valid-test

