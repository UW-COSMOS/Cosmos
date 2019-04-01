# Cosmos
Interface for applying Cosmos to document segmentation

Current milestone (with demo links): https://github.com/UW-COSMOS/project-docs/tree/master/presentations_reports/milestone_3

# Running the standalone images
We provide a docker image that includes everything necessary to run the model.



**NOTE**:
For performance and stability, it is *highly* recommended to run on a GPU. CPU running is heavily architecture-dependent. A `DEVICE` environment variable dictates which hardware to use,
either `DEVICE=cpu` or `DEVICE=cuda:0`

## Running on CPU:
To bypass dependency on GPU drivers and run the pipeline on CPU (with a performance penalty):
```
OUTPUT_DIR=./output/ INPUT_DIR=/path/to/input/docs DEVICE=cpu docker-compose -f docker-compose-standalone-CPU.yml up
```

## Running on a GPU:
Running the pipeline on GPU requires GPU drivers:
```
OUTPUT_DIR=./output/ INPUT_DIR=/path/to/input/docs DEVICE=cuda:0 docker-compose -f docker-compose-standalone-GPU.yml up
```





# Building + running the model
It is also possible to build the model image yourself. To do so:

1. Switch to the cosmos directory
2. Run, specifying the PDF input and desired output directories with the `INPUT_DIR` and `OUTPUT_DIR` environment variables, respectively

```
OUTPUT_DIR=./output/ INPUT_DIR=/path/to/input/docs DEVICE=cpu docker-compose up
```

# Layout of the model

Documentation can be found in cosmos/api_folder

The entry points for the program is cosmos/run.py

The procedure of the program is laid out generally as follows (docs correspond to to paths)

1. Preprocessing -- cosmos/preprocessing
    - Turn PDFs into PNGs so that they can be fed to a computer vision pipeline.
2. Create proposals -- cosmos/connected_components
    - Generate region proposals within document pages, this segments each page.
3. Ingesting data -- cosmos/ingestion
   - Prepare region proposals to be classified by a Neural Network as Body Text, Equation, Figure, etc.
4. Model inference -- Inference runner: cosmos/infer ||  Model definition: cosmos/model
   - Run the Neural Network on each region proposal.
5. Convert to HTML/XML --  cosmos/converters
   -  Results are converted to HTML/XML and class specific information extraction modules are run.
6. Postprocessing -- cosmos/postprocessing
   - Update class labels in light of extracted information.
7. Equation specific OCR -- cosmos/latex_ocr
   - Custom extraction pipeline for equations.
8. Create knowledge base of figures and tables -- cosmos/construct_caption_tables
9. Create knowledge base of equations -- cosmos/UnicodeParser
