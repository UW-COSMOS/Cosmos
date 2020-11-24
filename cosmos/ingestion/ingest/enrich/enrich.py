from typing import List
import pandas as pd
import re
import logging
import os
from tqdm import tqdm
import glob

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# use tqdm with df.progress_apply()
tqdm.pandas()


def run_enrich(input_path, output_path, dataset_id, spans, threshold):
    """
    Instantiate Enrich class and run context enrichment process over all parquets

    :param input_path a directory full of parquets (ingest output) to process
    :param output_path a directory to put the output context enriched parquets
    :param dataset_id ingest process dataset_id
    :param spans number of words each side of reference to table to capture for context
    :param threshold postprocess cls threshold for table and caption identification
    """
    for pq in glob.glob(os.path.join(input_path, '*.parquet')):
        logger.info(f'processing file: {pq}')
        df = pd.read_parquet(pq)
        enrich = Enrich(df, dataset_id)
        enrich.semantic_enrichment(output_path,
                                   spans=spans,
                                   threshold=threshold)
        basename = os.path.basename(pq)
        enrich.df.to_parquet(os.path.join(output_path, basename))


class Enrich:
    """enhance semantic content associated with tables/entities in ingestion pipeline output parquets"""
    def __init__(self, parquet_file, dataset_id, append=True):
        # TODO: revise for being called as dask job: scheduler_address, client=None,
        """
        load all ingest pipeline output files into dask dataframes
        :param parquet_file input parquet file_path
        :param dataset_id the string used as the dataset id to generate the .parquet files - should be in filenames
        :param append add results on to the input parquet
        """
        self.df = parquet_file
        self.original_cols = self.df.columns

        if append:
            # make a copy to append all rows to later.
            self.original_df = self.df.copy()

    def get_file_structure(self):
        """
        show first five rows of all found parquet files
        """
        # todo: delete this when done
        pd.options.display.max_columns = 999
        pd.options.display.max_colwidth = 250
        try:
            logger.info('parquet info:')
            logger.info(self.df.columns)
            logger.info(self.df.head())
        except AttributeError:
            logger.info('parquet attribute err')
            pass
        except NameError:
            logger.info('parquet name err - missing col?')
            pass

    def get_score_histogram(self, post_process_class: str, filepath: str):
        """
        output a histogram of the the postprocess scores for a given class
        intended to help set threshold variable

        :param post_process_class: class to examine e.g. Body Text, Table, Table Caption, ...
        :param filepath png file to output chart to"""

        ax = self.df[self.df['postprocess_cls'] == post_process_class].postprocess_score.plot.hist()
        ax.get_figure().savefig(filepath)

    def semantic_enrichment(self, filename: str, threshold: float = 0.9, spans: int = 20):
        """
        main method -calls all other processing methods and outputs enriched parquet file
        :param filename str parquet file to save results to
        :param threshold float cut off for postprocess table detection score to process as table caption
        :param spans number of words each side of label to pull in as context for each table label in content text
                if None will use regex to pull out full stop to full stop span around the table label
        """
        needed_column_names = [
                                'content',
                                'postprocess_cls',
                                'postprocess_score'
                              ]

        # only process every row if all needed column names are present in the parquet
        if all(x in self.df.columns for x in needed_column_names):
            logger.info('setting table labels')
            self.set_table_ids(threshold=threshold)

            logger.info('getting semantic context')
            self.create_semantic_contexts(spans=spans)

            logger.info('removing rows with no semantic context')
            self.df = self.df[self.df['semantic_context'].notna()]

            logger.info('replace content with context')
            self.replace_content_with_context()

            self .append_context_df_to_original_df()
        else:
            pass

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

        # append table_id to every row in dataframe if its a table or table caption and score meets threshold
        try:
            if ((row['postprocess_cls'] == 'Table Caption')
                    | (row['postprocess_cls'] == 'Table')) \
                    & (row['postprocess_score'] >= threshold):
                table_label = ' '.join(row['content'].split()[:2])
                if table_label:
                    output = table_label

        except KeyError:
            pass

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
        # get pdf content as a list
        words = pdf_content.split()

        # get all indexes in pdf content for the table_label
        label_indexes = []
        for i in range(len(words)-parts):
            if table_label == ' '.join(words[i:i+parts]):
                label_indexes.append(i)
        logger.debug(f'label_indexes: {label_indexes}')

        # get words either side of the label index, concatenate and return.
        result = []
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
                # suffix = words[i:i + len(words)]
                suffix = words[i:len(words)]

            result.append(' '.join(prefix + suffix))

        # output = ' '.join(output)
        logger.debug(result)
        return result

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
        result = re.findall(re_search_string, pdf_content)
        # re search returns list, make sure all are strings
        result = [str(x) for x in result]
        # concatenate list
        # output = ' '.join(output).strip()
        logger.debug(f'output type: {type(result)} output: {result}')
        return result

    def replace_content_with_context(self):
        """
        values of semantic_context column (each a list) are joined into a string and then replace the content column
        """
        self.df.loc[:, 'content'] = self.df.progress_apply(lambda x: ' '.join(x.loc['semantic_context']), axis=1)

    def append_context_df_to_original_df(self):
        """
        semantic content enriched df is limited to the original cols of the original dataframe, then appended to the
        original dataframe.
        """
        # add context rows to original df
        self.df = self.original_df.append(self.df[self.original_cols])
