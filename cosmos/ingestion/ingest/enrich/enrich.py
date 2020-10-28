from typing import List
import pandas as pd
import re
import logging
from dask import dataframe as dd
from os import listdir
from os.path import isfile, join
import scispacy
import spacy
from tqdm import tqdm

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# use tqdm with df.progress_apply()
tqdm.pandas()


class Enrich:
    """enhance semantic content associated with tables/entities in ingestion pipeline output parquets"""
    def __init__(self, parquet_files_dir, dataset_id, scispacy_models=['en_core_sci_md'], use_dask=False):
        """
        load all ingest pipeline output files into dask dataframes
        :param parquet_files_dir directory containing ingestion pipeline output .parquet files
        :param dataset_id the string used as the dataset id to generate the .parquet files - should be in filenames
        :param use_dask for very large .parquet files load dataframes as dask dataframes
        :param spacy_model specify a model from spaCy to use for NER
        """
        # get all files names in a specified dir
        list_of_files = [f for f in listdir(parquet_files_dir) if isfile(join(parquet_files_dir, f))]

        # load specific files that are US-COSMOS ingest pipeline output
        logger.info('files found:')
        if use_dask:
            logger.info('Loading files with Dask')
            for f in list_of_files:
                if f == dataset_id + '_pdfs.parquet':
                    logger.info(f'loading {f}')
                    self.df_pdfs = dd.read_parquet(join(parquet_files_dir, f))
                elif f == dataset_id + '_sections.parquet':
                    logger.info(f'loading {f}')
                    self.df_sections = dd.read_parquet(join(parquet_files_dir, f))
                elif f == dataset_id + '.parquet':
                    logger.info(f'loading {f}')
                    self.df = dd.read_parquet(join(parquet_files_dir, f))
                    
        else:
            for f in list_of_files:
                if f == dataset_id + '_pdfs.parquet':
                    logger.info(f'loading {f}')
                    self.df_pdfs = pd.read_parquet(join(parquet_files_dir, f))
                elif f == dataset_id + '_sections.parquet':
                    logger.info(f'loading {f}')
                    self.df_sections = pd.read_parquet(join(parquet_files_dir, f))
                elif f == dataset_id + '.parquet':
                    logger.info(f'loading {f}')
                    self.df = pd.read_parquet(join(parquet_files_dir, f))

        logger.info(f'loading spaCy models:')
        self.models = {}
        for name in tqdm(scispacy_models):
            self.models[name] = spacy.load(name)

    def get_file_structure(self):
        """
        show first five rows of all found parquet files
        """
        pd.options.display.max_columns = 999
        pd.options.display.max_colwidth = 250
        try:
            logger.info('dd_pdfs:')
            logger.info(self.df_pdfs.columns)
            logger.info(self.df_pdfs.head())
        except AttributeError:
            logger.info('no _pdfs output')
            pass
        except NameError:
            logger.info('no _pdfs output')
            pass

        try:
            logger.info('dd_sections:')
            logger.info(self.df_sections.columns)
            logger.info(self.df_sections.head())
        except AttributeError:
            logger.info('no _sections output')
            pass
        except NameError:
            logger.info('no _sections output')
            pass

        try:
            logger.info('df:')
            logger.info(self.df.columns)
            logger.info(self.df.head())
        except AttributeError:
            logger.info('no standard output')
            pass
        except NameError:
            logger.info('no standard output')
            pass

    def get_score_histogram(self, post_process_class, filepath):
        # post_process_class = 'Table Caption'
        ax = self.df[self.df['postprocess_cls'] == post_process_class].postprocess_score.plot.hist()
        ax.get_figure().savefig(filepath)

    def show_table_rows(self):
        pd.options.display.max_columns = 999
        pd.options.display.max_colwidth = 250

        # LOOK AT TABLE CAPTION ROW SCORES IN DF
        logger.info('df table captions')
        logger.info('post_process_score')
        max_vals = self.df[self.df['postprocess_cls'] == 'Table Caption'].postprocess_score.max(axis=0)
        min_vals = self.df[self.df['postprocess_cls'] == 'Table Caption'].postprocess_score.min(axis=0)
        mean_vals = self.df[self.df['postprocess_cls'] == 'Table Caption'].postprocess_score.mean(axis=0)
        logger.info(f'max:{max_vals}')
        logger.info(f'min:{min_vals}')
        logger.info(f'mean:{mean_vals}')

        # Show top 5 rows from parquet with table caption or table class
        logger.info(self.df.columns)
        logger.info(self.df[self.df['postprocess_cls'] == 'Table Caption'].head())
        logger.info('df tables:')
        logger.info(self.df[self.df['postprocess_cls'] == 'Table'].head())

    def semantic_enrichment(self, filename, threshold=0.9, spans=20):
        logger.info('setting table labels')
        self.set_table_ids(threshold=threshold)

        logger.info('removing rows with no table label')
        self.df = self.df[self.df['table_label'].notna()]

        logger.info('getting semantic context')
        self.create_semantic_contexts(spans=spans)

        logger.info('removing rows with no semantic context')
        self.df = self.df[self.df['semantic_context'].notna()]

        logger.info('getting named entities')
        self.create_entities_lists()


        logger.info(self.df[self.df['semantic_context'].notna()].head(10))

        self.export_data(filename)

    def set_table_ids(self, threshold):
        """
        identify and collect references for all tables in output parquets, per document
        column 'table_id' = <pdf_name><table_label>
        """
        # get all tables table ID is pdf, and table reference
        self.df['table_label'] = self.df.progress_apply(self.apply_table_labels, threshold=threshold, axis=1)

    def apply_table_labels(self, row, **kwargs):
        """
        Call this from df.apply()
        applied to a dataframe, return new column in data frame that == first two 'words' of content
        for each table caption
        """
        threshold = kwargs['threshold']
        output = None
        # append table_id to every row in dataframe if its a table caption and score meets threshold
        if (row['postprocess_cls'] == 'Table Caption') & (row['postprocess_score'] >= threshold):
            table_label = ' '.join(row['content'].split()[:2])
            if table_label:
                output = table_label

        return output

    def create_semantic_contexts(self, spans=None):
        """
        get the sentence with the reference, and the preceding sentence
        """
        self.df['semantic_context'] = self.df.progress_apply(self.get_semantic_context, spans=spans, axis=1)

    def get_semantic_context(self, row, **kwargs):
        """
        call this from df.apply - get body text that mentions the relevant table label
        :param row - passed to function when called from df.apply(), get row of dataframe to parse.
        :param spans - number of words either side of the table label to extract from pdf_content
            if spans == None, search is bases on regex comparison.
        returns output - for each row: text that contains the table label - sentence or span.
        """
        output = None
        spans = kwargs['spans']

        # for each row with a table label
        if row['table_label'] is not None:
            # get pdf for that table label
            table_pdf_name = row['pdf_name']
            table_label = row['table_label']
            logger.debug(f'table_pdf_name: {table_pdf_name}')
            logger.debug(f'table_label: {table_label}')

            # get all content in that pdf
            pdf_content = self.df[self.df['pdf_name'] == table_pdf_name].content.tolist()
            pdf_content = ' '.join(pdf_content)
            logger.debug(f'pdf_content: {pdf_content}')

            if spans:
                output = self.get_labels_and_spans_from_content(table_label, pdf_content, spans)
            else:
                output = self.get_labels_and_sentences_with_regex(table_label, pdf_content)

        # catch empty lists and convert to None
        if not output:
            output = None

        return output

    def get_labels_and_spans_from_content(self, table_label, pdf_content, spans):
        """
        :param table_label the label of the table to retrieve context for
        :param pdf_content current pdf content field
        :param spans the count of words to fetch as context on each side of the table_label
        return list of strings, table label in middle of each with span count of words from pdf content each side
        """
        # find label in content, and return spans # of words from either side

        # get number of words/parts in table label
        parts = len(table_label.split())
        # get pdf   content as a list
        words = pdf_content.split()

        # get all indexes in pdf content for the table_label
        label_indexes = []
        for i in range(len(words)-parts):
            if table_label == ' '.join(words[i:i+parts]):
                label_indexes.append(i)
        logger.debug(f'label_indexes: {label_indexes}')

        # get words either side of the label index, concatenate and return.
        output = []
        for i in label_indexes:
            try:
                prefix = words[i-spans:i]
            except IndexError:
                # if start of pdf_content is < # of words in span from table label mention, just go from start
                prefix = words[0:i]

            try:
                suffix = words[i:i+spans]
            except IndexError:
                # if end of pdf_content is < # of words in span from table label mention, just go to the end
                suffix = words[i:i+len(words)]

            output.append(' '.join(prefix+suffix))

        # output = ' '.join(output)
        logger.debug(output)
        return output

    def get_labels_and_sentences_with_regex(self, table_label: str, pdf_content: str) -> List[str]:
        """
        :param table_label
        :param pdf_content
        returns list of string each the slice between two full stops containing the table label
        """
        # for each content find table label and return sentence containing it (from start . to end .)

        # strip non alpha numerics from table label
        # prevent special characters being interpreted as additional regex components
        # keeps spaces and full stops though
        pattern = re.compile("[^0-9a-zA-Z. ]+")
        table_label = pattern.sub('', table_label)

        # use regex to find table label and return string between two fullstops either side of it
        re_search_string = r"([^.]*?"+table_label+r"[^.]*\.)"
        output = re.findall(re_search_string, pdf_content)
        # re search returns list, make sure all are strings
        output = [str(x) for x in output]
        # concatenate list
        # output = ' '.join(output).strip()
        logger.debug(f'output type: {type(output)} output: {output}')
        return output

    def create_entities_lists(self):
        """
        detect named entities in semantic context
        """
        for model_name, model in self.models.items():
            self.df['named_entities_'+model_name] = self.df.progress_apply(self.get_named_entities, model=model, axis=1)

    def get_named_entities(self, row, **kwargs) -> List:
        """
        return all named entities in a list per row passed
        :param row - row from dataframe
        :return list of named entities
        """
        ners = []
        model = kwargs['model']
        # get list of semantic contexts from row
        # iterate over contexts, finding entities, appending to list. One list for all contexts.
        for cont in row['semantic_context']:
            doc = model(cont)
            for ent in doc.ents:
                # logger.debug(f'dir(ent): {dir(ent)}')
                # logger.debug(f'{ent.text}, {ent.start_char}, {ent.end_char}, {ent.label}')
                ners.append([ent.text, ent.label_])

        # if empty list return None
        if not ners:
            ners = None

        return ners

    def export_data(self, filename):
        """
        output parquet file with only semantic enhancement for table captions rows
        """
        logger.info(f'outputting {filename}')
        self.df[self.df['semantic_context'].notna()].to_parquet(filename)