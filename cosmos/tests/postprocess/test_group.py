"""
Testing for group_cls
"""

import ipdb
from postprocess.postprocess import group_cls
import unittest

def test_basic_merge():
    obj_list = [('Table', [10, 10, 20, 20], 1), ('Table', [25, 10, 35, 20], 1),
                ('Table', [10, 25, 20, 35], 1), ('Table', [25, 25, 35, 35], 1)]
    expected_list = [('Table', [10, 10, 35, 35], 1)]
    actual_list = group_cls(obj_list, 'Table')
    assert expected_list == actual_list

def test_basic_merge_fail():
    obj_list = [('NotTable', [10, 10, 20, 20], 1), ('Table', [25, 10, 35, 20], 1),
                ('Table', [10, 25, 20, 35], 1), ('NotTable', [25, 25, 35, 35], 1)]
    expected_list = [('NotTable', [10, 10, 20, 20], 1), ('Table', [25, 10, 35, 20], 1),
                ('Table', [10, 25, 20, 35], 1), ('NotTable', [25, 25, 35, 35], 1)]
    actual_list = group_cls(obj_list, 'Table')
    unittest.TestCase().assertCountEqual(expected_list, actual_list)

def test_merge_one_leave_two():
    obj_list = [('Table', [10, 10, 20, 20], 1), ('Table', [25, 10, 35, 20], 1),
                ('Table', [10, 25, 20, 35], 1), ('NotTable', [25, 25, 35, 35], 1)]
    expected_list = [('Table', [10, 10, 35, 20], 1), ('Table', [10, 25, 20, 35], 1),                     ('NotTable', [25, 25, 35, 35], 1)]

    actual_list = group_cls(obj_list, 'Table')
    unittest.TestCase().assertCountEqual(expected_list, actual_list)

def test_merge_two_leave_one():
    obj_list = [('Table', [10, 10, 20, 20], 1), ('Table', [25, 10, 35, 20], 1),
                ('Table', [10, 25, 20, 35], 1), ('Table', [25, 25, 35, 35], 1),
                ('NotTable', [10, 22, 35, 23], 1)]
    expected_list = [('Table', [10, 10, 35, 20], 1), ('NotTable', [10, 22, 35, 23], 1), ('Table', [10, 25, 35, 35], 1)]
    actual_list = group_cls(obj_list, 'Table')
    unittest.TestCase().assertCountEqual(expected_list, actual_list)


