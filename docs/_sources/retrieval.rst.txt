Retrieval
=========

After we ingest our higher level objects to ElasticSearch, we can use all of the associated tools that come with
ElasticSearch to retrieve objects.

More recently, two stage retrieval systems, which deploy a deep learning reranking model on top of retrieved document
results, have shown to be better performing for both basic information retrieval and also for downstream tasks such
as question answering.

We also deploy a two stage reranking model. Our reranker uses BERT-Large as its base architecture, and by default is
trained on MS-Marco. On our roadmap is easily training this model on user feedback.

Our setting is slightly different from the traditional document retrieval setting, and also different from settings
such as question answering. Like question answering, we are retrieving relatively short contexts, but the contexts are
composed of not pure text sequences. Also unlike question answering, we are seeking to return interesting information,
not necessarily the specific answer to a user's query.

With this last point in mind, we prioritize diversity in returned PDFs. To do this, we use ElasticSearch to retrieve
a set of N documents, given all the text content in that document. We find all objects of the
type defined by the query on these returned documents. We then run reranking on all these objects.

Instead of returning this reranked list, we choose to filter the list such that only the top ranking object for each
of the initial N documents remains. In this way, we end up with a ranked list of the initial N documents based on
how informative the "most" informative object in that document is.

For pagination, we paginate at the document level. If you retrieve the first 25 documents, you will get a ranked list
of those 25 documents according to ElasticSearch + reranking. If you ask for more results, the next page of objects
will come from the next 25 documents, and thus will have no overlap with the first page of results.

In this way, you can scroll through hundreds of documents, finding fresh relevant objects to explore.




Elasticsearch Index - Fields
============================


+---------------------+-----------+---------------------------------------------------------------------------------+
| Field               | Type      | Description                                                                     |
+=====================+===========+=================================================================================+
|  Object fields                                                                                                    |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "area"              | "integer" | "size (in sq. pixels) of identified area"                                       |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "cls"               | "text"    | "detected object class (figure, table, body text, section header, etc)"         |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "content"           | "text"    | "Text content within the detected object"                                       |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "dataset_id"        | "text"    | "(COSMOS internal) - Identifier for dataset."                                   |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "detect_score"      | "float"   | "Score (strength) of detected classification prediciton"                        |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "full_content"      | "text"    | "Combined content of the object and its associated parent/header"               |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "header_content"    | "text"    | "Text content of the associated parent/header object (caption, section header)" |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "img_pth"           | "text"    | "Path to image file on disk"                                                    |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "pdf_name"          | "text"    | "Name of source PDF"                                                            |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "postprocess_score" | "float"   | "Confidence of postprocess detection correction process"                        |
+---------------------+-----------+---------------------------------------------------------------------------------+
|  Entity fields                                                                                                    |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "aliases"           | "text"    | "Aliases for known entities"                                                    |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "canonical_id"      | "text"    | "Canonical ID for known entities (e.g. UMLS id)"                                |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "description"       | "text"    | "Description of known entities"                                                 |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "name"              | "text"    | "Name of known entity"                                                          |
+---------------------+-----------+---------------------------------------------------------------------------------+
| "types"             | "keyword" | "Type of entity"                                                                |
+---------------------+-----------+---------------------------------------------------------------------------------+
