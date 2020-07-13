"""
This retriever does not currently adhere to the Retriever interface
"""
from pyserini.search import pysearch

class PyseriniRetriever(Retriever):
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

    def search(self, query, start_index=0):
        k = 10 # Number of contexts to retrieve
        doc_hits = self.doc_searcher.search(query, k=self.k1)
        results = []
        doc_ids = []
        for i in range(0, min(len(doc_hits), self.k1)):
            index = doc_hits[i].docid
            doc_ids.append(int(self.doc2odoc_map[int(index)]))

        context_hits = self.context_searcher.search(query, k=self.k2)

        for i in range(start_index, min(len(context_hits), self.k2)):
            index = context_hits[i].docid
            content = context_hits[i].raw
            score = context_hits[i].score
            actual_context_id = self.ctx2octx_map[int(index)]
            actual_doc_id = self.octx2odoc_map[int(actual_context_id)]
            if actual_doc_id in doc_ids:
                results.append([actual_doc_id, actual_context_id, content, score, i])
        return results

