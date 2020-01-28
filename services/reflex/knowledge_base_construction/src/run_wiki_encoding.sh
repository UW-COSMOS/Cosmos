#for SPLIT in train valid; do \
#    python multiprocessing_bpe_encoder.py \
#        --encoder-json /data/encoder.json \
#        --vocab-bpe /data/vocab.bpe \
#        --inputs /data/${SPLIT}.txt \
#        --outputs /data/${SPLIT}.bpe \
#        --keep-empty \
#        --workers 60; \
#done

fairseq-preprocess \
    --only-source \
    --srcdict /data/dict.txt \
    --trainpref /data/train.bpe \
    --validpref /data/valid.bpe \
    --destdir /data/bin/ \
    --workers 60

