.. _existing-es-cluster:

Reading data into an existing ElasticSearch cluster
----------------------------------------------------

If you already have an existing ElasticSearch cluster setup and do not wish to spawn an additional one, in
docker-compose-api.yml, remove the es01 service.

Then in your .env file, redefine the following environment variable:

.. code-block:: bash

    ELASTIC_ADDRESS=your.elasticsearch.address

The retrieval and upload images will then look at the updated ELASTIC_ADDRESS.

