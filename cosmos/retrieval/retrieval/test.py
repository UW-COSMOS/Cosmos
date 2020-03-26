from retrieve import Retrieval

if __name__ == "__main__":
    dataset_id = 'a721488d-9c8b-49a3-8db0-c97f87ce7ad3'
    doc_idx_path = '/index_dir/' + dataset_id + '/documents_jsonl/lucene-index'
    context_idx_pth = '/index_dir/' + dataset_id + '/contexts_jsonl/lucene-index'
    doc2odoc_pth = '/index_dir/' + dataset_id + '/id_to_pdf.pkl'
    ctx2octx_pth = '/index_dir/' + dataset_id + '/id_to_context.pkl'
    octx2odoc_pth = '/index_dir/'+ dataset_id + '/context_to_doc.pkl'
    retrieval = Retrieval(doc_idx_path, context_idx_pth, doc2odoc_pth, ctx2octx_pth, octx2odoc_pth, k1=1, k2=1)
    result = retrieval.search('Late Devonian reef')
    # Result is a list of list : [[actual_doc_id, actual_context_id, content, score], [actual_doc_id, actual_context_id, content, score], ....]
    print(result)