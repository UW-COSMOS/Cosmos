#!/bin/bash
# Extract vocabulary
echo 'Starting to learn BPE'
subword-nmt learn-bpe -s 20000 < $1 > /app/corpus/codes.txt
echo 'Applying BPE'
subword-nmt apply-bpe -c /app/corpus/codes.txt < $1 | subword-nmt get-vocab > /app/corpus/vocab.L
# Now apply vocabulary filter
echo 'Apply vocab filter'
subword-nmt apply-bpe -c /app/corpus/codes.txt --vocabulary /app/corpus/vocab.L --vocabulary-threshold 50 < $1 > /app/corpus/vocab.bpe
