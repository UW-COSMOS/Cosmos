# Usage : /src/index.sh <path_to_dataset>
./src/anserini/target/appassembler/bin/IndexCollection -collection JsonCollection \
    -generator LuceneDocumentGenerator -threads 9 -input $1/collection_jsonl \
    -index $1/lucene-index -storePositions -storeDocvectors -storeRawDocs