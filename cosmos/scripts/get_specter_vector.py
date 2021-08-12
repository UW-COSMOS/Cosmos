import cosmos.api.cosmos.retrieval as r
from transformers import AutoTokenizer, AutoModel
from typing import List


class SPECTERizer:
    """convert list of docids to list of SPECTER vectors"""
    def __init__(self):
        # load model and tokenizer
        self.tokenizer = AutoTokenizer.from_pretrained('allenai/specter')
        self.model = AutoModel.from_pretrained('allenai/specter')

    def get_vectors(self, document_list: List[str]) -> List[List[float]]:
        """
        fetch bibjsons for docids, extract title and abstracts, return 768 element SPECTER vector per docid
        https://arxiv.org/pdf/2004.07180.pdf
        https://github.com/allenai/specter
        :param: document_list
        :return: List
        """
        # fetch then concatenate title and abstract
        title_abs = []
        for document_json in r.get_bibjsons(document_list):
            try:
                title = document_json['title']
            except(KeyError, TypeError):
                title = ''
            try:
                abstract = document_json['abstract']
            except(KeyError, TypeError):
                abstract = ''
            title_abs.append(title + self.tokenizer.sep_token + abstract)

        print('concatenated titles and abstracts:')
        print(title_abs)

        # tokenize title_abs and get model output
        inputs = self.tokenizer(title_abs, padding=True, truncation=True, return_tensors="pt", max_length=512)
        result = self.model(**inputs)
        # take the first token in the batch as the embedding, the SPECTER vector
        return result.last_hidden_state[:, 0, :].tolist()


def main():
    document_list = ['123.pdf', '124.pdf', '125.pdf']
    sp = SPECTERizer()
    specter_vectors = sp.get_vectors(document_list)
    print(specter_vectors)


if __name__ == "__main__":
    main()
