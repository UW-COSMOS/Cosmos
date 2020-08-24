export OMP_NUM_THREADS=1
conda update conda
conda install -y \
    nomkl \
    pytorch torchvision cudatoolkit=10.1 numpy pandas dask scikit-learn sqlalchemy click beautifulsoup4 tqdm pyarrow tensorboard scikit-image xgboost opencv pdfminer.six tensorboardx gunicorn flask -c pytorch -c conda-forge && conda clean -afy
pip install pascal-voc-writer pytesseract pikepdf hyperyaml transformers elasticsearch_dsl

