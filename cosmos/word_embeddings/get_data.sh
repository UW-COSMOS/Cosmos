#!/bin/sh

mkdir data
cd data
curl -O https://geodeepdive.org/app_output/corpus_1k.zip
unzip corpus_1k.zip
rm corpus_1k.zip
