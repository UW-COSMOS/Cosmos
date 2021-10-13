import pandas as pd
from transformers import AutoTokenizer, AutoModel
from pathlib import Path
from typing import List, Tuple


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

    @staticmethod
    def _convert_to_single_doc_dfs(df:pd.DataFrame) -> List[pd.DataFrame]:
        """convert df of multiple pdfs to list of dfs each addressing a single pdf"""
        all_pdf_names = list(df.pdf_name.unique())
        single_doc_dfs = []
        for name in all_pdf_names:
            single_doc_dfs.append(df[df['pdf_name'] == name])
        return single_doc_dfs

    def _extract_title_and_abstract(self, single_doc_dfs: List[pd.DataFrame]) -> Tuple[Tuple[str], Tuple[str]]:
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
        return zip(*title_abs.items())

    def _vectorize(self, title_abs: Tuple[str]) -> List[List[float]]:
        """
        get title and abstract per pdf in parquet return df columns pdf_name and specter vector

        get row title and abstracts, return 768 element SPECTER vector
        https://arxiv.org/pdf/2004.07180.pdf
        https://github.com/allenai/specter
        :param row: pandas DataFrame row - i.e. use df.apply(get_vectors)
        :return: list[float] a SPECTER vector
        """

        # tokenize title_abs and get model output
        inputs = self.tokenizer(list(title_abs), padding=True, truncation=True, return_tensors="pt", max_length=512)
        result = self.model(**inputs)
        # take the first token in the batch as the embedding, the SPECTER vector
        return result.last_hidden_state[:, 0, :].tolist()

    @staticmethod
    def _build_df(doc_ids: Tuple[str], specter_vectors: List[List[float]]) -> pd.DataFrame:
        return pd.DataFrame.from_dict(
            {
                'pdf_name': list(doc_ids),
                'SPECTER_vectors': specter_vectors
            }
        )

    def create_vectors(self):
        """load parquet create SPECTER vectors parquet and save to disk"""
        document_sections_df = pd.read_parquet(self.filepath)
        single_doc_dfs = self._convert_to_single_doc_dfs(document_sections_df)
        doc_ids, title_abs = self._extract_title_and_abstract(single_doc_dfs)
        specter_vectors = self._vectorize(title_abs)
        vector_df = self._build_df(doc_ids, specter_vectors)
        vector_df.to_parquet(self.filepath.parent / (self.dataset_id + '_specter_vectors.parquet'))


def main():
    # demonstrate specter vector production and output format
    # set up input _sections.parquet
    filepath = Path.cwd()
    dataset_id = 'documents'
    input_parquet_path = filepath / (dataset_id + '_sections.parquet')
    pd.DataFrame.from_dict(
        {
            'pdf_name': ['123', '123', '124', '124', '125', '125'],
            'section_header': [None, 'abstract', None, 'Abstract', None, 'ABSTRACT'],
            'content': ['COVID-19 research report', 'Here we summarise some COVID-19 research',
                        'Immunization response analysis', 'mRNA goes in, antibodies come out',
                        'COVID-19 impact on public transport', 'The wheels on the bus go round and round']
        }
    ).to_parquet(input_parquet_path)

    # run SPECTERizer
    sp = SPECTERizer(filepath=filepath,
                     dataset_id=dataset_id)
    sp.create_vectors()

    # show result
    output_parquet_path = filepath / (dataset_id + '_specter_vectors.parquet')
    print(pd.read_parquet(output_parquet_path))

    # tidy up - remove input and output parquets
    input_parquet_path.unlink()
    output_parquet_path.unlink()


if __name__ == "__main__":
    main()
