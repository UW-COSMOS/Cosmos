"""
Utility for ingesting a file into a dataframe
used by the evaluation code
Author:Josh McGrath
"""
from converters.xml2list import xml2list
import pandas as pd

def ingest_file(path):
    """
    Ingest an XML file to a dataframe
    :param path: path to XML file
    :return: dataframe of [id, label, x0,y0,x1,y1]
    """
    lst = xml2list(path)
    labels = [item[0] for item in lst]
    coords = [item[1] for item in lst]
    scores = [float(item[2]) for item in lst]
    x0 = [coord[0] for coord in coords]
    y0 = [coord[1] for coord in coords]
    x1 = [coord[2] for coord in coords]
    y1 = [coord[3] for coord in coords]
    return pd.DataFrame({
            "label": labels,
            "x0": x0,
            "x1": x1,
            "y0": y0,
            "y1": y1,
            "score": scores
        })


