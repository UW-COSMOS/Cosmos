python multiprocessing_bpe_encoder.py --encoder-json /data/encoder.json --vocab-bpe /data/vocab.bpe --inputs /data/output_train_capitals.txt --outputs /data/output_train.bpe --keep-empty --workers 60
fairseq-preprocess --only-source --srcdict /data/dict.txt --trainpref /data/output_train.bpe --destdir /data/bin/ --workers 60
