from retrieval.elastic_reranking_retriever import ElasticRerankingRetriever
from extraction.qa_extractor import QAExtractor
import pandas as pd


def run():
    ret = ElasticRerankingRetriever('tcp://localhost:8786')
    qa_model = QAExtractor('tcp://localhost:8786')
    print('Building index')
    ret.build_index('/ssd/ankur/contracts/c_pdfs.parquet', '/ssd/ankur/contracts/c_sections.parquet')
    print('Done building index.')
    while True:
        q = input('Input a query (type "end" to end):')
        if q == 'end':
            break
        results = ret.search(q)[:5]
        data = []
        for ind, result in enumerate(results):
            docname = result['docname']
            c = result['context']
            answer, score = qa_model.extract(q, c)
            data.append({'Rank': ind+1, 'PDF Name': docname, 'Long Answer': c[:50] + '...', 'Short Answer': answer[:50] + '...'})
        df = pd.DataFrame(data)
        print(df.to_markdown())

    ret.delete(dataset_id='none')

if __name__ == '__main__':
    run()
