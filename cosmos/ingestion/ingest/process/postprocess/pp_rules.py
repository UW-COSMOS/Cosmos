import yaml
import joblib
import re
import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)

def rule_caption(text, cls):
    """
    Objective: If first word is "Figure" or "Table" followed by a number, assume it's a caption.
    """

    new_cls = cls
    first_line = text.split("\n")[0]
    matches = re.findall('^(figure|fig)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', first_line, flags=re.IGNORECASE|re.MULTILINE)
    if len(matches) >0:
        new_cls = "Figure Caption"
        logging.info(f"Figure caption detected. Used to be {cls}")

    matches = re.findall('^(table|tbl|tab)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', first_line, flags=re.IGNORECASE|re.MULTILINE)
    if len(matches) >0 and cls != 'Table':
        new_cls = "Table Caption"
        logging.info(f"Table caption detected. Used to be {cls}")

    return new_cls

def apply_rules(page_objs):
    new_objs = []
    for obj in page_objs:
        bb, cls, text, score = obj
        new_cls = rule_caption(text, cls)
        new_objs.append((bb, cls, text, score, new_cls))
    return new_objs
