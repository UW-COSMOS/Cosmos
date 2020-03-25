"""
Information Retrieval using anserinin given a query
"""
from pyserini.search import pysearch
import pickle

class Retriever:
    def __init__(self, doc_idx_path, context_idx_pth, doc2odoc_pth, ctx2octx_pth, octx2odoc_pth, k1=5, k2=100):
        self.doc_searcher = pysearch.SimpleSearcher(doc_idx_path)
        self.context_searcher = pysearch.SimpleSearcher(context_idx_pth)
        with open(doc2odoc_pth, 'rb') as handle:
            self.doc2odoc_map = pickle.load(handle)
        with open(ctx2octx_pth, 'rb') as handle:
            self.ctx2octx_map = pickle.load(handle)
        with open(octx2odoc_pth, 'rb') as handle:
            self.octx2odoc_map = pickle.load(handle)
        self.k1 = k1
        self.k2 = k2

    def search(query):
        k = 10 # Number of contexts to retrieve
        doc_hits = self.doc_searcher.search(query, k=self.k1)
        results = []
        doc_ids = []
        for i in range(0, max(len(doc_hits), self.k1)):
            index = doc_hits[i].docid
            doc_ids.append(self.doc2odoc_map[index])

        context_hits = self.context_searcher.search(query, k=self.k2)
        for i in range(0, max(len(context_hits), self.k2)):
            index = hits[i].docid
            content = hits[i].content
            score = hits[i].score
            actual_doc_id = self.octx2odoc_map[self.ctx2octx_map[index]]
            if actual_doc_id in doc_ids:
                results.append([actual_doc_id, content, score])
        return results
