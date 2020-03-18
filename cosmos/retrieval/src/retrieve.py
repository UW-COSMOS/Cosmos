"""
Information Retrieval using anserinin given a query
"""
from pyserini.search import pysearch

def search():
    index_dir = "natural_questions/dev/lucene-index-msmarco" # Replace this with <dataset>/lucene-index
    query = "where did they film high school musical two" # replace this with actual query
    searcher = pysearch.SimpleSearcher(index_dir)
    k = 10 # Number of contexts to retrieve
    hits = searcher.search(query, k=k)
    for i in range(0, k):
        index = hits[i].docid
        content = hits[i].content
        score = hits[i].score
        print("index : ", index, ", content : ", content, ", score : ", score)

if __name__ == "__main__":
    search()
