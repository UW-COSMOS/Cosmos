import pandas as pd
import re
import logging
from dask import dataframe as dd
from os import listdir
from os.path import isfile, join

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


class Enrich:
    """enhance semantic content associated with tables/entities in ingestion pipeline output parquets"""
    def __init__(self, parquet_files_dir, dataset_id, use_dask=False):
        """
        load all ingest pipeline output files into dask dataframes
        """
        # get all files names in a specified dir
        list_of_files = [f for f in listdir(parquet_files_dir) if isfile(join(parquet_files_dir, f))]

        # load specific files that are US-COSMOS ingest pipeline output
        logger.info('files found:')
        if use_dask:
            for f in list_of_files:
                logger.info('using dask')
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

    def semantic_enrichment(self, filename):
        logger.info('setting table labels')
        self.set_table_ids()
        # logger.info(self.df[self.df['table_label'].notna()].head())

        logger.info('getting semantic context')
        self.create_semantic_contexts()
        logger.info(self.df[self.df['semantic_context'].notna()].head(10))

        self.export_data(filename)

    def set_table_ids(self):
        """
        identify and collect references for all tables in output parquets, per document
        column 'table_id' = <pdf_name><table_label>
        """
        # get all tables table ID is pdf, and table reference
        self.df['table_label'] = self.df.apply(self.apply_table_labels, threshold=0.9, axis=1)

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

    def create_semantic_contexts(self):
        """
        get the sentence with the reference, and the preceding sentence
        """
        self.df['semantic_context'] = self.df.apply(self.get_semantic_context, axis=1)

    def get_semantic_context(self, row):
        """
        call this from df.apply - get body text that mentions the relevant table label
        """
        output = None

        # for each row with a table label
        if row['table_label'] is not None:
            # get pdf for that table label
            table_pdf_name = row['pdf_name']
            table_label = row['table_label']

            # strip non alpha numerics from table label
            # prevent special characters being interpreted as additional regex components
            # keeps spaces and full stops though
            pattern = re.compile("[^0-9a-zA-Z. ]+")
            table_label = pattern.sub('', table_label)

            logger.info(f'table_pdf_name: {table_pdf_name}')
            logger.info(f'table_label: {table_label}')

            # get all content in that pdf
            pdf_content = self.df[self.df['pdf_name'] == table_pdf_name].content.tolist()
            pdf_content = ' '.join(pdf_content)
            logger.info(f'pdf_content: {pdf_content}')

            # for each content find table label and return sentence containing it (from start . to end .)
            # TODO: change from regex to spans
            re_search_string = r"([^.]*?"+table_label+r"[^.]*\.)"
            output = re.findall(re_search_string, pdf_content)
            # re search returns list, make sure all are strings
            output = [str(x) for x in output]
            # concatenate list
            output = ' '.join(output).strip()
            logger.info(f'output type: {type(output)} output: {output}')

        return output

    def export_data(self, filename):
        """
        output parquet file with only semantic enhancement for table captions rows
        """
        logger.info(f'outputting {filename}')
        self.df[self.df['semantic_context'].notna()].to_parquet(filename)