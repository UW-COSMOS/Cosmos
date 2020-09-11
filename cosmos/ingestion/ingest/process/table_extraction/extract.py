import camelot

def extract_tables(df):
    pdfs = df.groupby('pdf_name')
    for pdf, group in pdfs:
        tables = camelot.read_pdfs(pdf)
