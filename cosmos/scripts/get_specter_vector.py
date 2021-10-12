import pandas as pd
from transformers import AutoTokenizer, AutoModel
from pathlib import Path


class SPECTERizer:
    """add SPECTER vector parquet to COSMOS output parquets"""
    def __init__(self, filepath: str, dataset_id: str):
        """filepath is dir parquet files e.g. COSMOS output dir - load _sections.parquet"""
        # load model and tokenizer
        self.tokenizer = AutoTokenizer.from_pretrained('allenai/specter')
        self.model = AutoModel.from_pretrained('allenai/specter')

        # handle file location
        self.dataset_id = dataset_id
        self.filepath = Path(filepath)
        if self.filepath.exists() and self.filepath.is_dir():
            self.filepath = self.filepath / (dataset_id+'_sections.parquet')
        else:
            raise NotADirectoryError()

    def create_vectors(self):
        """load parquet create SPECTER vectors parquet and save to disk"""
        document_sections_df = pd.read_parquet(self.filepath)
        specter_vectors = self._vectorize(document_sections_df)
        specter_vectors.to_parquet(self.filepath.parent / (self.dataset_id+'_specter_vectors.parquet'))

    def _vectorize(self, df:pd.DataFrame) -> pd.DataFrame:
        """
        get title and abstract per pdf in parquet return df columns pdf_name and specter vector

        get row title and abstracts, return 768 element SPECTER vector
        https://arxiv.org/pdf/2004.07180.pdf
        https://github.com/allenai/specter
        :param row: pandas DataFrame row - i.e. use df.apply(get_vectors)
        :return: list[float] a SPECTER vector
        """
        # get list of dfs each addressing a single pdf
        all_pdf_names = list(df.pdf_name.unique())
        single_doc_dfs = []
        for name in all_pdf_names:
            single_doc_dfs.append(df[df['pdf_name'] == name])

        # get title and abstract where possible from each single pdf df
        title_abs = {}
        for doc in single_doc_dfs:
            pdf_name = doc.iloc[0]['pdf_name']
            try:
                title = doc.iloc[0]['content']  # assuming first row for a pdf in _section parquet is the title
            except IndexError:
                title = ''
            try:
                abstract = doc[doc['section_header'].str.lower() == 'abstract']['content'].values[0]
            except IndexError:
                abstract = ''

            if (title and abstract) or (title or abstract):
                title_abs[pdf_name] = (title + self.tokenizer.sep_token + abstract)

        # split ids and title abs, retain order
        doc_ids, title_abs = zip(*title_abs.items())

        # tokenize title_abs and get model output
        inputs = self.tokenizer(list(title_abs), padding=True, truncation=True, return_tensors="pt", max_length=512)
        result = self.model(**inputs)
        # take the first token in the batch as the embedding, the SPECTER vector
        result = result.last_hidden_state[:, 0, :].tolist()

        vector_df = pd.DataFrame.from_dict(
            {
                'pdf_name': list(doc_ids),
                'SPECTER_vectors': result
            }
        )
        return vector_df


def main():
    sp = SPECTERizer(filepath='/Users/imcconnell/UWCOSMOS/table_extraction/parquets_for_testing/',
                     dataset_id='documents')
    sp.create_vectors()


if __name__ == "__main__":
    main()
