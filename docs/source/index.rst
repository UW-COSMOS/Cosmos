.. Cosmos documentation master file, created by
   sphinx-quickstart on Fri Jun 26 16:30:11 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Cosmos
==================================

Cosmos is a library to automate the retrieval of information from PDF documents. While created with the intention of
automating the process of scientific discovery and analysis, the components can be applied generally to stacks of
documents.

Cosmos is composed of three parts:

1. **Ingestion** of information in PDF documents. Cosmos automates the process of extracting text, tables, figures, and
other components commonly found in PDF documents.

2. **Retrieval** of information across stacks of documents. Cosmos is packaged with several algorithms used to
build and access search indices over the ingested PDFs.

3. **Extraction** of information. Cosmos enables the extraction of relational information using state of the art language
and question answering models.

.. toctree::
   :maxdepth: 2
   :caption: Contents

   ingest.rst
   retrieval.rst
   extraction.rst
