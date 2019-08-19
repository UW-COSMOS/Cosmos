#!/bin/sh

mkdir data
cd data
curl -O https://geodeepdive.org/app_output/word2vec_output.zip
unzip word2vec_output.zip
rm word2vec_output.zip
