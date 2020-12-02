from typing import List
import pandas as pd
import re
import logging
import os
from tqdm import tqdm
import glob
import functools

import signal
from dask.distributed import Client, progress, as_completed

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# use tqdm with df.progress_apply()
tqdm.pandas()


def needed_columns_are_in_df(to_check: List, to_interrogate: List):
    # True if all of list_to_check values are in list_to_interrogate
    if all(x in to_interrogate for x in to_check):
        return True
    else:
        return False


class Enrich:
    """
    Enrich class
    handles running semantic enrichment of tables in ingestion pipeline output parquets
    """
    def __init__(self, scheduler_address, client=None):
        # TODO: revise for being called as dask job: scheduler_address, client=None,
        """
        :param scheduler_address: Address to existing Dask scheduler
        :param client: A Dask client. Can be passed in. If None, one will be created to connect to the scheduler
        """
        logger.info("Initializing Enrich object")
        self.client = client
        if self.client is None:
            logger.info("Setting up client")
            self.client = Client(scheduler_address, serializers=['msgpack', 'dask'], deserializers=['msgpack', 'dask', 'pickle'])
            logger.info(self.client)

    def __del__(self):
        """Simple client clean up"""
        if self.client is not None:
            self.client.close()

    def enrich(self, input_path: str, output_path: str, dataset_id: str,
               threshold: float = 0.8, spans: int = 20):
        """
        main method -calls all other processing methods and outputs enriched parquet file
        :param input_path: a directory full of parquets (ingest output) to process
        :param output_path: a directory to put the output context enriched parquets
        :param dataset_id: ingest process dataset_id
        :param threshold: float cut off for postprocess table detection score to process as table caption
        :param spans: number of words each side of label to pull in as context for each table label in content text
                if None will use regex to pull out full stop to full stop span around the table label
        """

        for pq in glob.glob(os.path.join(input_path, '*.parquet')):
            logger.info(f'processing file: {pq}')
            df = pd.read_parquet(pq)
            basename = os.path.basename(pq)

            needed_columns = [
                                'content',
                                'postprocess_cls',
                                'postprocess_score'
                             ]

            if needed_columns_are_in_df(needed_columns, list(df.columns)):

                if dataset_id:
                    logger.info(f'limit enrichment to dataset id: {dataset_id}')
                    df = df[df['dataset_id'] == dataset_id]

                # GET ALL DOCUMENTS, LIST OF DFs
                pdf_names = list(df.pdf_name.unique())
                single_doc_dfs = []

                logger.info('split ingest output into docs')
                for name in tqdm(pdf_names):
                    single_doc_dfs.append(df[df['pdf_name'] == name])

                partial_get_context = functools.partial(Enrich.get_contexts, threshold, spans)
                logger.info('start enrichment processing')
                enriched = [self.client.submit(partial_get_context, doc_df, resources={'process': 1}) for doc_df in single_doc_dfs]
                progress(enriched)
                logger.info('collecting all enriched docs')
                enriched = [e.result() for e in tqdm(enriched)]
                df = pd.concat(enriched)
                df = df.reset_index(drop=True)

            else:
                pass

        df.to_parquet(os.path.join(output_path, basename))

    @classmethod
    def get_contexts(cls, threshold, spans, doc_df):
        # disable SettingWithCopyWarning - safe since always over writing original
        pd.options.mode.chained_assignment = None
        original_df = doc_df.copy()

        label_length = 2
        classes = ['Table', 'Table Caption']

        # get first label_length words of every content cell in df if it's classified correctly and scored highly enough
        logger.info('setting table labels')
        doc_df['table_label'] = [
                                 ' '.join(content.split()[:label_length])
                                 if (pp_cls in classes) and (pp_score >= threshold)
                                 else None
                                 for content, pp_cls, pp_score in zip(doc_df['content'],
                                                                      doc_df['postprocess_cls'],
                                                                      doc_df['postprocess_score'])
                                ]

        # if table_label series has no values, just return original df
        if len(doc_df[doc_df["table_label"].astype(bool)]) == 0:
            return original_df

        # aggregate all doc words into one list of words
        words = ' '.join(doc_df.content.tolist()).split()

        # mark where each label occurs in list of all doc words
        doc_df['indices'] = [
                                [
                                    j
                                    for j in range(len(words))
                                    if table_label == ' '.join(words[j:j + label_length])
                                ]
                                for table_label in doc_df['table_label']
                            ]

        # iterate over list in indices column to extract words before labels in content
        logger.info('getting semantic context')
        doc_df['prefix'] = [
                                [
                                    ' '.join(words[i - spans:i])
                                    if i - spans >= 0
                                    else ' '.join(words[0:i])
                                    for i in index
                                ]
                                for index in doc_df['indices']
                           ]

        # iterate over list in indices column to extract words after labels in content
        doc_df['suffix'] = [
                                [
                                    ' '.join(words[i+label_length:i+label_length + spans])
                                    if i+label_length + spans <= len(words)
                                    else ' '.join(words[i+label_length:len(words)])
                                    for i in index
                                ]
                                for index in doc_df['indices']
                           ]

        # join prefixes and suffixes together
        doc_df['context'] = [
                                [
                                    prefix[i] + ' ' + suffix[i]
                                    for i in range(len(prefix))
                                ]
                                for prefix, suffix in zip(doc_df['prefix'], doc_df['suffix'])
                            ]

        # remove rows without context (empty lists)
        logger.info('removing rows with no semantic context')
        doc_df = doc_df[doc_df['context'].apply(lambda x: len(x) > 0)]

        # replace content with context as text only
        logger.info('replace content with context')
        doc_df['content'] = [' '.join(x) for x in doc_df['context']]

        # append context only df to original df using only original columns
        logger.info('append context to original and return')
        return original_df.append(doc_df.loc[:, list(original_df.columns)])
