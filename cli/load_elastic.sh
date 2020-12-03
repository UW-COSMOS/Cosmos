#!/bin/bash
python3.8 -m retrieval.scripts.build_elastic_index \
  --host es01 \
  --sections-parquet  /output/documents_sections.parquet \
  --documents-parquet /output/documents_pdfs.parquet \
  --tables-parquet    /output/documents_tables.parquet \
  --figures-parquet   /output/documents_figures.parquet \
  --equations-parquet /output/documents_equations.parquet \
  --entities-parquet  /output/documents_entities.parquet
