"""
Retriever interface
"""

class Retriever:
    def search(self, query):
        raise NotImplementedError('Use a subclass of Retriever')

    def rerank(self, query, contexts):
        raise NotImplementedError('Use a subclass of Retriever')

    def build_index(self, input_path):
        raise NotImplementedError('Use a subclass of Retriever')

    def delete(self, dataset_id):
        raise NotImplementedError('Use a subclass of Retriever')

