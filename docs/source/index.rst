.. Cosmos documentation master file, created by
   sphinx-quickstart on Fri Jun 26 16:30:11 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Cosmos
==================================

Cosmos is an open source semantic search engine that focuses on the retrieval of information from PDF documents.
While created with the intention of automating the process of scientific discovery and analysis, the components
can be applied generally to stacks of documents.

Cosmos is composed of three parts:

1. **Ingestion** of information in PDF documents. Cosmos automates the process of extracting text, tables, figures, and
other components commonly found in PDF documents.

2. **Retrieval** of information across stacks of documents. Cosmos utilizes ElasticSearch and an optional
neural reranker.

3. **Extraction** of information. Cosmos is also packaged with a question answering model that can be deployed
to answer queries with subspans retrieved from the retrieval module.


.. toctree::
   :maxdepth: 2
   :caption: Contents

   getting_started.rst
   ingest.rst
   retrieval.rst
   extraction.rst
   docker_builds.rst
   existing_es.rst
