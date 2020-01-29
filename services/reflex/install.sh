#!/bin/bash
echo running install script;
#pip install --editable /fairseq
pip install pexpect --upgrade
python -m spacy download en_core_web_lg
tail -F /dev/null

