# COSMOS
Cosmos is an open source semantic search engine that focuses on the retrieval of information from PDF documents. While created with the intention of automating the process of scientific discovery and analysis, the components can be applied generally to stacks of documents.

Cosmos is composed of three parts:

1. Ingestion of information in PDF documents. Cosmos automates the process of extracting text, tables, figures, and other components commonly found in PDF documents.

2. Retrieval of information across stacks of documents. Cosmos utilizes ElasticSearch and an optional neural reranker.

3. Extraction of information. Cosmos is also packaged with a question answering model that can be deployed to answer queries with subspans retrieved from the retrieval module.

See https://uwcosmos.github.io/Cosmos for full documentation.
 
# License and Acknowledgements
All development work supported by DAPRA ASKE HR00111990013 and UW-Madison.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this repo except in compliance with the License.
You may obtain a copy of the License at:

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
