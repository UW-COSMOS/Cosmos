"""
Convert an html file back into an xml document (for evaluation purposes)
"""
from bs4 import BeautifulSoup
import re
import os
import glob
import codecs
from .postprocessing.postprocessing import not_ocr




