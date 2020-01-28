#!/bin/bash
echo running install script;
pip install --editable /drqa
pip install --editable /lama
pip install --editable /fairseq
pip install pexpect --upgrade
python -m spacy download en_core_web_sm
pip install spacy
tail -F /dev/null
nginx

