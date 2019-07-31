# Cosmos
Interface for applying Cosmos to document segmentation

Current milestone (with demo links): https://github.com/UW-COSMOS/project-docs/tree/master/presentations_reports/milestone_3

# Running the standalone images
We provide a separate repo (https://github.com/UW-COSMOS/cosmos-demo) describing how to use our canonical docker images, which include everything necessary to run the model.


# Building + running the model from scratch
It is also possible to build the model image yourself. To do so:

1. Switch to the cosmos directory
2. Run, specifying the PDF input and desired output directories with the `INPUT_DIR` and `OUTPUT_DIR` environment variables, respectively

```
OUTPUT_DIR=./output/ INPUT_DIR=/path/to/input/docs DEVICE=cpu docker-compose up
```

# Layout of the model

Documentation can be viewed at https://uw-cosmos.github.io/Cosmos/

The entry points for the program is cosmos/run.py

The procedure of the program is laid out generally as follows (docs correspond to paths)

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

# Installing locally
1. Install kubectl (https://kubernetes.io/docs/tasks/tools/install-kubectl/)
2. Install minikube (https://kubernetes.io/docs/tasks/tools/install-minikube/) (if already installed, run minikube delete)
3. Install Helm (https://helm.sh/docs/using_helm/#quickstart)
4. Clone repo on branch v0.2.0 (https://github.com/UW-COSMOS/Cosmos/tree/v0.2.0)
5. Head to the COSMOS root folder.
6. Run (minikube start --memory 12288 --cpus 4 --disk-size 20g)
7. Install dependencies (helm init)
8. Ensure pods are running (kubectl get pods)
9. Run this command a few times (helm repo update)
10. Go to the services dir (cd services). Install MongoDB (install_mongo.sh)
11. Retrieve the mongodb password: (export MONGODB_ROOT_PASSWORD=$(kubectl get secret --namespace default cosmos-mongodb -o jsonpath=“{.data.mongodb-root-password}” | base64 --decode))
12. Run the clusters (kubectl run)
13. Go to proposals dir in services (cd proposals). Run (install.sh)
14. Ensure clusters are still running (kubectl get pods)
15. Head to the detection service (cd ../detection)
16. Change the values.yaml file (Pull Policy: IfNotPresent)
17. SSH into the minikube (minikube ssh)
18. Pull the docker files (docker pull ankurgos/detection:test). SSH out once done
19. Go to the ingestion folder (cd ../ingestion)
20. Change the values.yaml file (pdfDir: <Your Folder>)
21. Run (install.sh)
22. Go to src (cd src). Run (python pdf_ingestion.py)

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
