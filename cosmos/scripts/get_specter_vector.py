import cosmos.api.cosmos.retrieval as retrieval
from transformers import AutoTokenizer, AutoModel
from typing import List, Tuple

DocidVectorTupleList = List[Tuple[str, List[float]]]


class SPECTERizer:
    """convert list of docids to list of SPECTER vectors"""
    def __init__(self):
        # load model and tokenizer
        self.tokenizer = AutoTokenizer.from_pretrained('allenai/specter')
        self.model = AutoModel.from_pretrained('allenai/specter')

    def get_vectors(self, doc_ids: List[str]) -> DocidVectorTupleList:
        """
        fetch bibjsons for docids, extract title and abstracts, return 768 element SPECTER vector per docid
        https://arxiv.org/pdf/2004.07180.pdf
        https://github.com/allenai/specter
        :param: document_list
        :return: Dict of {docid: SPECTER vectors list}
        """
        # fetch then concatenate title and abstract
        title_abs = {}

        bibjsons = retrieval.get_bibjsons(doc_ids)

        if not bibjsons:
            print('No bibjsons found!')
            return None

        for doc_id, document_json in bibjsons.items():
            try:
                title = document_json['title']
            except(KeyError, TypeError):
                title = ''
            try:
                abstract = document_json['abstract']
            except(KeyError, TypeError):
                abstract = ''
            title_abs[doc_id] = (title + self.tokenizer.sep_token + abstract)

        # split ids and title abs, retain order
        fetched_doc_ids, fetched_title_abs = zip(*title_abs.items())

        # tokenize title_abs and get model output
        inputs = self.tokenizer(list(fetched_title_abs), padding=True, truncation=True, return_tensors="pt", max_length=512)
        result = self.model(**inputs)
        # take the first token in the batch as the embedding, the SPECTER vector
        result = result.last_hidden_state[:, 0, :].tolist()

        # re attach appropriate doc ids, return list of tuples
        return list(zip(list(fetched_doc_ids), result))


def main():
    document_list = ['577d6dd3cf58f12e60eaaa37', '5eec4d71a58f1dfd52099852', '577fde75cf58f13158ed150f']
    sp = SPECTERizer()
    specter_vectors = sp.get_vectors(document_list)
    print(specter_vectors)


if __name__ == "__main__":
    main()
