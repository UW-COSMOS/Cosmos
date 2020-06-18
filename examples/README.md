#Examples

## Setup

To run the examples, first initialize a Python 3.6 environment. For example using miniconda:

```bash
conda create -n cosmos python=3.6
conda activate cosmos
```

Then install the requirements in this directory via pip

```bash
pip install -r requirements.txt
```

Finally, you'll need [ghostscript](https://www.ghostscript.com/) installed. On Mac OS X using Homebrew:

```bash
brew install ghostscript
```

## Barebones Detection Pipeline

An example of the detection pipeline up to the deep learning model's output can be found in detection.ipynb.

