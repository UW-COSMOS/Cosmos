Getting Started
==================================

To get started with Cosmos, we highly suggest utilizing some of our prebuilt _Docker images.
These handle building dependencies and running your documents through the pipeline without
having to worry about installing things locally.

First, clone the Cosmos repository, and change into the root directory of the repository.
Open a file named .env in the directory, and populate it with the following:

.. code-block:: bash
    BASE_IMAGE=ankurgos/cosmos-base:latest
    DETECT_IMAGE=ankurgos/cosmos-ingest:latest
    WORKER_IMAGE=ankurgos/cosmos-ingest:latest
    RETRIEVAL_IMAGE=ankurgos/cosmos-retrieve:latest
    EXTRACTION_IMAGE=ankurgos/cosmos-extract:latest
    VISUALIZER_IMAGE=uwcosmos/visualizer_kb:latest
    DETECT_PROCS=1
    WORKER_PROCS=7
    INPUT_DIR=/path/to/input/dir
    TMP_DIR=/path/to/tmp/dir
    OUTPUT_DIR=/path/to/output/dir

The first six lines in the file define which images to use. The default cosmos images assumes access to a CUDA
enabled GPU. To utilize a CPU, append to each image "-cpu". For example, change ankurgos/cosmos-base:latest to
ankurgos/cosmos-base-cpu:latest.

Depending on your machine, you can scale the process by setting DETECT_PROCS and WORKER_PROCS to the desired number of
processes.

Finally, make sure to set the final three directories, denoting the input directory pointing to your PDFs, a temporary
directory with sufficient hard drive space to write images and such, and an output directory to write information.

To process the images, run the following line:

.. code-block:: bash
    docker-compose -f deployment/docker-compose-ingest.yml up

The output directory you defined will now be populated with a set of _Parquet files, as well as an images directory
containing object specific images.

Deploying the COSMOS search interface
===================================

To deploy the search interface over this processed dataset, run the following command:

.. code-block:: bash
    docker-compose -f deployment/docker-compose-api.yml up


You then should be able to navigate to localhost:8082 in your browser to access the search interface.

.. _Docker: https://www.docker.com/
.. _Parquet: https://parquet.apache.org/
