from pdfminer.pdfparser import PDFParser
from pdfminer.pdfdocument import PDFDocument
from pdfminer.pdfinterp import PDFResourceManager
from pdfminer.layout import LTTextBox, LTText, LTTextLine, LTChar
from pdfminer.pdfinterp import PDFPageInterpreter
from pdfminer.pdfpage import PDFPage
from pdfminer.layout import LAParams
from pdfminer.converter import PDFPageAggregator

import pandas as pd

def update_pos(pos1,pos2):
    x1 = min(pos1[0],pos2[0])
    y1 = min(pos1[1],pos2[1])
    x2 = max(pos1[2],pos2[2])
    y2 = max(pos1[3],pos2[3])
    return (x1,y1,x2,y2)

def parse_pdf(fp):
    with open(fp, "rb") as fh:
        parser = PDFParser(fh)
        doc = PDFDocument(parser)
        laparams = LAParams()
        rsrcmgr = PDFResourceManager()
        device = PDFPageAggregator(rsrcmgr=rsrcmgr, laparams=laparams)
        interpreter = PDFPageInterpreter(rsrcmgr, device)
        texts = []
        text = ''
        positions = []
        pos = (10000,10000,-1,-1)
        pages = []
        for idx, page in enumerate(PDFPage.create_pages(doc)):
            interpreter.process_page(page)
            layout = device.get_result()
            #print(layout.bbox)
            for child in layout:
                if isinstance(child, LTTextBox):
                    for line in child:
                        for char in line:
                            if isinstance(char, LTChar):
                                text += char.get_text()
                                pos = update_pos(char.bbox,pos)
                                page = idx
                            else:
                                texts.append(text)
                                positions.append(pos)
                                pages.append(page)
                                text = ''
                                pos = (10000,10000,-1,-1)
                          
    x1, y1, x2, y2 = list(zip(*positions))
    df = pd.DataFrame({
        "text": texts,
        "x1": x1,
        "y1":y1,
        "x2": x2,
        "y2":y2,
        "page": pages
                       })
    return df, layout.bbox
