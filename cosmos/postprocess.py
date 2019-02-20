from bs4 import BeautifulSoup
import re
import os
import glob
import codecs

def not_ocr(text):
    """
    TODO: Docstring for not_ocr.

    Args:
        text (TODO): TODO

    Returns: TODO

    """
    return 'ocr' not in text and 'rawtext' not in text

def check_caption_body(soup):
    """
    Check for things that look like captions that are flagged as body test.

    Simply looks for "figure", "fig", "table", or "tab" plus a number at the start of a line

    Args:
        soup (bs4.BeautiulSoup): The page's html, in soup form

    Returns: Corrected soup
    """
    for seg_type in soup.find_all('div', not_ocr):
        seg_class = " ".join(seg_type["class"])
        lines = seg_type.find_all('span', 'ocr_line')
        if len(lines) > 0:
            for line in lines:
                clean_line = line.getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
                matches = re.findall('^(figure|fig)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
                if len(matches) >0:
                    seg_type["class"] = "Figure Caption"
                matches = re.findall('^(table|tbl|tab)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
                if len(matches) >0:
                    seg_type["class"] = "Table Caption"
    return soup

def postprocess(html_path, output_path):
    for f in glob.glob(os.path.join(html_path, "*.html")):
        with codecs.open(os.path.join(output_path, os.path.basename(f)), "w", "utf-8") as fout:
            with codecs.open(f, "r", "utf-8") as fin:
                soup = BeautifulSoup(fin, 'html.parser')
                new_soup = check_caption_body(soup)
                print(f"Writing to {output_path}")
                fout.write(str(new_soup))
