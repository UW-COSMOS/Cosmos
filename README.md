# Cosmos
Interface for applying Cosmos to document segmentation

# Running the model

1. Switch to the cosmos directory
2. Run, specifying the PDF input directory with the `INPUT_DIR` environment variable

```
INPUT_DIR=/path/to/input/docs DEVICE=cpu docker-compose up
```
# Layout of the model

Documentation can be found in cosmos/api_folder

The entry points for the program is cosmos/run.py

The procedure of the program is laid out generally as follows (docs correspond to to paths)

1. Preprocessing -- cosmos/preprocessing
2. Create proposals -- cosmos/connected_components
3. Ingesting data -- cosmos/ingestion
4. Run model inference -- Inference runner: cosmos/infer ||  Model definition: cosmos/model
5. Converters to convert to HTML/XML -- After inference, results are converted to HTML/XML: cosmos/converters
6. Postprocessing -- cosmos/postprocessing
7. Equation specific OCR -- cosmos/latex_ocr
8. Create knowledge base of figures and tables -- cosmos/construct_caption_tables
9. Create knowledge base of equations -- cosmos/UnicodeParser

