'''
Test html2xml
'''

from ...converters.html2xml import html2xml
from ...converters.xml2list import xml2list
import filecmp
import os
import glob
import unittest

def teardown_function(function):
    for f in glob.glob('tests/converters/xml_predict/*'):
        os.remove(f)

def test_html2xml():
    html2xml('tests/converters/html', 'tests/converters/xml_predict')
    for f in glob.glob('tests/converters/xml_predict/*'):
        xml_list = xml2list(f, feather=False)
        predict_list = [x[1] for x in xml_list]
        basename = os.path.basename(f)
        test_f = os.path.join('tests', 'converters', 'xml', basename)
        test_xml_list = xml2list(test_f)
        expected_list = [x[1] for x in test_xml_list]
        unittest.TestCase().assertCountEqual(predict_list, expected_list)
        os.remove(f)



