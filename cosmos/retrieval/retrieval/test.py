from retrieve import Retrieval
import os

if __name__ == "__main__":
    dataset_id = os.environ['DATASET_ID']
    doc_idx_path = '/index_dir/' + dataset_id + '/documents_jsonl/lucene-index'
    context_idx_pth = '/index_dir/' + dataset_id + '/contexts_jsonl/lucene-index'
    doc2odoc_pth = '/index_dir/' + dataset_id + '/id_to_pdf.pkl'
    ctx2octx_pth = '/index_dir/' + dataset_id + '/id_to_context.pkl'
    octx2odoc_pth = '/index_dir/'+ dataset_id + '/context_to_doc.pkl'
    retrieval = Retrieval(doc_idx_path, context_idx_pth, doc2odoc_pth, ctx2octx_pth, octx2odoc_pth, k1=1, k2=1)
    result = retrieval.search('explosive')
    for r in result:
        print(len(r))
        print(r[0], r[1])
    # Result is a list of list : [[actual_doc_id, actual_context_id, content, score], [actual_doc_id, actual_context_id, content, score], ....]
    #print(result)
