Getting Started
==================================

To get started with Cosmos, we highly suggest utilizing some of our prebuilt _Docker images.
These handle building dependencies and running your documents through the pipeline without
having to worry about installing things locally.

First, clone the Cosmos repository, and change into the root directory of the repository.
Open a file named .env in the directory, and populate it with the following:

.. code-block:: console

    BASE_IMAGE=uwcosmos/cosmos-base
    DETECT_IMAGE=uwcosmos/cosmos-ingestion
    WORKER_IMAGE=uwcosmos/cosmos-ingestion
    RETRIEVAL_IMAGE=uwcosmos/cosmos-retrieval
    EXTRACTION_IMAGE=ankurgos/cosmos-extraction:latest
    VISUALIZER_IMAGE=uwcosmos/visualizer_kb:latest
    UPLOAD_IMAGE=iaross/cosmos-api:latest
    API_IMAGE=iaross/cosmos-api:latest
    LINKING_IMAGE=uwcosmos/cosmos-linking
    DETECT_PROCS=1
    WORKER_PROCS=8
    DEVICE=cuda
    RERANKING_DEVICE=cuda
    #DEVICE=cpu
    #RERANKING_DEVICE=cpu
    SCHEDULER_ADDRESS=scheduler:8786
    ELASTIC_ADDRESS=es01:9200    
    INPUT_DIR=/path/to/input/dir
    TMP_DIR=/path/to/tmp/dir
    OUTPUT_DIR=/path/to/output/dir
    ELASTIC_DATA_PATH=/path/to/es/directory

The first seven lines in the file define which images to use. The default cosmos images assumes access to a CUDA
enabled GPU. To utilize a CPU, append to each image "-cpu". For example, change uwcosmos/cosmos-base:latest to
uwcosmos/cosmos-base-cpu:latest. If you use the CPU version, make sure to change all \*_DEVICE from 'cuda' to 'cpu'.

Depending on your machine, you can scale the process by setting DETECT_PROCS and WORKER_PROCS to the desired number of
processes.

Finally, make sure to set the final four directories, denoting:
1. an input directory pointing to your PDFs (all pdfs will need to  be renamed to valid docids)
2. a temporary directory with sufficient hard drive space to write images and such
3. an output directory to write information. 
4. Note that the directory serving as ELASTIC_DATA_PATH will need its permissions set to allow read/write by any user, in order to accommodate ElasticSearch's permissions model (e.g. sudo chmod 777 /path/to/es/directory).
5. create a directory images/ within your output directory so your user has access to all the output.

If working on a remote server, launch tmux or other multiplexing if you haven't already as you will need several panes to run several docker-compose applications.

To process the images, run the following line:

.. code-block:: console

    docker-compose -f deployment/docker-compose-ingest.yml -p cosmos up

The docker-compose ingest application will process all the documents and then just idle when it is finished. You can ctrl-c out of it at that stage.

The output directory you defined will now be populated with a set of Parquet_ files, as well as an images directory
containing object specific images, and saved word embeddings over the input corpus. 

In the output images directory (/path/to/output/dir/images) run the following to move all files into folders beginning with the first two chars of their file names:

.. code-block:: console

    for f in *.png; do [ ! -d ${f:0:2} ] && mkdir ${f:0:2}; mv $f ${f:0:2}/$f; done

Entity linking
--------------
Once the documents have been ingested, a separate process can be run to recognize named entities
within the extracted objects, linking them to the Unified Medical Language System (UMLS). Named
entity recognition and UMLS linking are accomplished via SciSpacy_. The parquet files will be
modified to include linked and unlinked entities, and an additional parquet file will be created
containing canonical information for the linked entities. To run the entity linking pipeline:

.. code-block:: console

    docker-compose -f deployment/docker-compose-link.yml -p cosmos up

Deploying the COSMOS search interface
-------------------------------------

To deploy the search interface over this processed dataset, run the following command:

.. code-block:: console

    docker-compose -f deployment/docker-compose-api.yml cosmos up

This will create an elasticsearch service for you. If you already have a ElasticSearch cluster online, see
:ref:`Existing ElasticSearch Cluster <existing-es-cluster>`. Its datapath is will be set to the environment variable set in the .env file.

You can expect to see five images up while this is running with docker ps. They are named:

1. cosmos_rerank_model_1
2. cosmos_front_end_1
3. cosmos_scheduler_1
4. cosmos_birdnest_backend
5. es01

If you see an es01 failure in the logging at this stage it is likely due to the permissions not being set appropriately on the ELASTIC_DATA_PATH as mentioned above. See the :doc:`troubleshooting` guide for details.

Keep the docker-compose API application running in it's own pane/terminal. Start a new pane/terminal for the next step.

Reading ingested data into ElasticSearch
----------------------------------------

The files outputted by ingestion must now be read into ElasticSearch indices. In a separate window, run:

.. code-block:: console

    docker-compose -f deployment/docker-compose-api-upload.yml cosmos up

This could take some time, depending on the size of your document stack.

Note that the ElasticSearch data will persist even if you re-run the ingest, api and upload applications. To make sure you are only retrieving on the latest ElasticSearch data: delete, re-create, and reset the permissions on the ELASTIC_DATA_PATH directory between cycles.

Viewing output
--------------

You should now be able to navigate to localhost:8082 in your browser to access the search interface.

.. _Docker: https://www.docker.com/
.. _Parquet: https://parquet.apache.org/
.. _ElasticSearch: https://www.elastic.co/home
.. _SciSpacy: https://allenai.github.io/scispacy/

Problems
--------

Common errors/issues and their solutions are detailed here in the :doc:`troubleshooting` guide.
