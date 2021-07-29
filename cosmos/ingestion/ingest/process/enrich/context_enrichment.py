"""
using output ingest parquets update <dataset_id>_tables.parquet with context_from_text column
"""

import functools
import os
import glob
import dask
from dask.distributed import progress
import pandas as pd
from tqdm import tqdm
from typing import Tuple
import re
import logging

logging.basicConfig(format='%(levelname)s :: %(filename) :: %(funcName)s :: %(asctime)s :: %(message)s',
                    level=logging.ERROR)
logger = logging.getLogger(__name__)
logger.setLevel(logging.ERROR)


def context_enrichment(file_path: str, dataset_id: str, pp_threshold: float, d_threshold: float,
                       spans: int, client: dask.distributed.Client, qa: bool = False):
    """
    add rows to dataset_id_tables.parquet
    :param file_path: a directory full of parquets (ingest output) to process
    :param dataset_id: ingest process dataset_id
    :param pp_threshold: float cut off for postprocess_score to process as table caption
    :param d_threshold: float cut off for detect_score to process as table caption
    :param spans: number of words each side of label to pull in as context for each table label in content text
            if None will use regex to pull out full stop to full stop span around the table label
    :param client: dask distributed client to pass jobs to
    :param qa: if qa process should run in contexts
    """

    dataset_id_df_path = ''
    tables_df_path = ''
    for pq in glob.glob(os.path.join(file_path, '*.parquet')):
        if os.path.basename(pq) == dataset_id + '.parquet':
            dataset_id_df_path = pq
        if os.path.basename(pq) == dataset_id + '_tables.parquet':
            tables_df_path = pq

    logger.info(f'getting all pdfs with tables: {tables_df_path}')
    table_file_name = os.path.basename(tables_df_path).split('.')[0]
    tables_df = pd.read_parquet(tables_df_path)
    pdf_names_with_tables = list(set(tables_df['pdf_name'].values.tolist()))

    # Get the 'documents.parquet', only rows from docs with tables
    dataset_id_df = pd.read_parquet(dataset_id_df_path)
    dataset_id_df = dataset_id_df[dataset_id_df['pdf_name'].isin(pdf_names_with_tables)]

    single_doc_dfs = []
    table_rows_per_doc_dfs = []
    logger.info('split ingest output into docs')
    for name in tqdm(pdf_names_with_tables):
        single_doc_dfs.append(dataset_id_df[dataset_id_df['pdf_name'] == name])
        table_rows_per_doc_dfs.append(tables_df[tables_df['pdf_name'] == name])

    partial_get_contexts = functools.partial(get_contexts,
                                             pp_threshold,
                                             d_threshold,
                                             spans,
                                             qa)
    logger.info(f'start enrichment processing with doc count {len(single_doc_dfs)}')
    enriched = [client.submit(partial_get_contexts,
                              doc_and_tables_dfs,
                              resources={'process': 1})
                for doc_and_tables_dfs in zip(single_doc_dfs, table_rows_per_doc_dfs)]
    progress(enriched)
    logger.info('collecting all enriched docs')
    enriched = [e.result() for e in enriched]

    # enriched is a list of tuples - split into list of lists.
    enriched = list(map(list, list(zip(*enriched))))

    logger.info('dump enriched results:')
    logger.info(f'{enriched}')

    missed_references = enriched[1]
    statistics = enriched[2]
    enriched = enriched[0]

    df = pd.concat(enriched)
    logger.info(f'size of df returned from enrichment: {len(df)}')
    df = df.reset_index(drop=True)

    output_path = os.path.join(file_path, table_file_name + '.parquet')
    logger.info(f'outputting data: {output_path}')
    df.to_parquet(output_path)

    if qa:
        if not all(v is None for v in missed_references):
            missed_references = pd.concat(missed_references)
            # parquet column names must be strings
            missed_references.rename(
                {x: 'column_' + str(x) for x in list(missed_references.columns)},
                axis=1,
                inplace=True
            )
            logger.info(f'docs with missed_references df: {len(missed_references)}')
            output_path = os.path.join(file_path, 'missing_table_references.parquet')
            logger.info(f'outputting missing table references: {output_path}')
            logger.info(f'data frame to dump to parquet: {missed_references}')
            missed_references.to_parquet(output_path)
        if not all(v is None for v in statistics):
            statistics = pd.concat(statistics)
            output_path = os.path.join(file_path, 'table_detection_statistics_summary.parquet')
            statistics.describe().round(3).to_parquet(output_path)
            output_path = os.path.join(file_path, 'table_detection_statistics_per_pdf.parquet')
            statistics.to_parquet(output_path)


def get_contexts(pp_threshold: float, d_threshold: float, spans: int, qa: bool,
                 doc_and_tables_dfs: Tuple[pd.DataFrame, pd.DataFrame]) -> Tuple[pd.DataFrame,
                                                                                 pd.DataFrame,
                                                                                 pd.DataFrame]:
    """
    perform context enrichment per doc in ingest output parquet - code to run on dask cluster worker
    :param pp_threshold: postprocess_score value needed to act on a given Table or Table Caption
    :param d_threshold: detect_score value needed to act on a given Table or Table Caption
    :param spans: number of words either side of a label to capture as context in doc content
    :param doc_and_tables_dfs: input dataframes - representing one doc of output from ingest pipeline, and
    associated tables
    :param qa: if QA process should run in context enrichment, return missing table information and statistics
    :param doc_and_tables_dfs: a tuple of a single pdf's COSMOS output consisting of all the rows for that pdf from
    the non-aggregated parquet and the tables parquet, each as a pd.DataFrame
    :return tables_df, missing_refs, statistics_df: input table dataframe with any enriched rows added,
    df of pdf_name with any tables missed, df with pdf name and table detect recall, precision, and f1
    :return type: Tuple(pd.DataFrame, pd.DataFrame, pd.DataFrame)
    """

    # disable SettingWithCopyWarning - safe since always over writing original
    pd.options.mode.chained_assignment = None
    label_length = 2
    context_column = 'context_from_text'
    missing_refs = None
    statistics_df = None

    # aggregate all doc words into one list of words, excepting tables and table captions
    doc_df = doc_and_tables_dfs[0]
    pp_classes_to_ignore = ['Table', 'Table Caption']
    all_doc_words = []
    for index, row in doc_df.iterrows():
        if row['postprocess_cls'] not in pp_classes_to_ignore:
            all_doc_words.extend(row['content'].split())

    # get table rows df and iteration copy, set pdf_name for error reporting
    tables_df = doc_and_tables_dfs[1]
    tables_df[context_column] = None  # create empty enriched content column
    original_tables_df = tables_df.copy()  # copy to iterate over, copy to update
    tables_df_columns = tables_df.columns.tolist()  # columns list for outputting final df
    pdf_name = original_tables_df.pdf_name.to_list()[0]

    # join hyphenations in all words:
    dehyphenated_words = []
    i = 0
    try:
        while i < len(all_doc_words):
            if all_doc_words[i][-1] == '-':
                if i + 1 <= len(all_doc_words):
                    combine = (all_doc_words[i] + all_doc_words[i + 1]).replace("-", '')
                    dehyphenated_words.append(combine)
                    i += 2
                else:
                    dehyphenated_words.append(all_doc_words[i])
                    i += 1
            else:
                dehyphenated_words.append(all_doc_words[i])
                i += 1
    except (IndexError):
        logger.info(f'DE-HYPHENATION FAILED ON {pdf_name} at index {i}: {all_doc_words[i]}')

    all_doc_words = dehyphenated_words
    label_match_pattern = 'table' + r'( ){1,2}[0-9.ivxlcdm-]+[\w]?'

    # attempt to update empty 'caption_contents' from 'content'
    try:
        original_tables_df['caption_content'] = [
            re.sub(r"( ){1,}", " ", re.search(label_match_pattern, content.lower()).group(0))
            if caption_content is None and
               (pp_score >= pp_threshold) and
               (d_score >= d_threshold) and
               (len(content.split()) >= label_length) and
               re.search(label_match_pattern, content.lower()) is not None
            else caption_content
            for caption_content, pp_score, d_score, content in zip(original_tables_df['caption_content'],
                                                                   original_tables_df['postprocess_score'],
                                                                   original_tables_df['detect_score'],
                                                                   original_tables_df['content'])
        ]
    except Exception as e:
        logger.info(f"{pdf_name} failed updating caption_contents:\n {e}")
        logger.info(f'table dump:\n {original_tables_df}')

    # attempt to update caption_contents values with no table label
    try:
        original_tables_df['caption_content'] = [
            re.sub(r"( ){1,}",
                   " ",
                   re.search(label_match_pattern,
                             content.lower()
                             ).group(0)
                   ) + ' ' + caption_content
            if caption_content is not None and
               (pp_score >= pp_threshold) and
               (d_score >= d_threshold) and
               (len(content.split()) >= label_length) and
               re.search(label_match_pattern, content.lower()) is not None and
               re.search(label_match_pattern, caption_content.lower()) is None
            else caption_content
            for caption_content, pp_score, d_score, content in zip(original_tables_df['caption_content'],
                                                                   original_tables_df['postprocess_score'],
                                                                   original_tables_df['detect_score'],
                                                                   original_tables_df['content'])
        ]
    except Exception as e:
        logger.info(f"{pdf_name} failed updating caption_contents:\n {e}")
        logger.info(f'table dump:\n {original_tables_df}')

    # set label on each table column row:
    # assume first two tokens are valid labels if strip all non alphanumeric from left.
    try:
        original_tables_df['table_label'] = [
            re.sub(r"[^\w]+$", '', ' '.join(caption_content.split()[:label_length]).lower())
            if caption_content and
               (pp_score >= pp_threshold) and
               (d_score >= d_threshold)
            else None
            for caption_content, pp_score, d_score in zip(original_tables_df['caption_content'],
                                                          original_tables_df['postprocess_score'],
                                                          original_tables_df['detect_score'])
        ]
    except Exception as e:
        logger.info(f"{pdf_name} failed updating table labels:\n {e}")
        logger.info(f'table dump:\n {original_tables_df}')

    table_labels_from_COSMOS = original_tables_df['table_label'].tolist()
    logger.info(f"table references from COSMOS {pdf_name}: {table_labels_from_COSMOS}")

    if qa:
        # get all unique table references in text and compare to all table mentions in table df
        # return df with index='pdfname': columns=[co-references in text not mentioned in table df]

        # find all table reference locations in all body text, incl figure 11.11
        table_reference_indices_from_text = [
            j
            for j in range(len(all_doc_words))
            if re.match(r'^([^\w]?){0,}' + 'table ' + r'[0-9.ivxlcdm-]+([^\w]?){0,}$',
                        ' '.join(all_doc_words[j:j + label_length]).lower())
        ]

        # get text at those locations
        table_references_from_text = [' '.join(all_doc_words[j:j + label_length]).lower()
                                      for j in table_reference_indices_from_text]
        logger.debug(f'table references from text {pdf_name}: {table_references_from_text}')

        # remove trailing non-alphanumeric characters from references
        table_references_from_text = [
            'table' + re.sub(r"[^\w]+$", '', x.split('table')[1])
            for x in table_references_from_text]
        # unique list
        table_references_from_text = list(set(table_references_from_text))
        logger.info(f'clean unique references from text {pdf_name}:{table_references_from_text}')

        # list of table reference from cosmos
        table_labels_from_COSMOS = [table_ref.lower()
                                    if table_ref
                                    else None
                                    for table_ref in table_labels_from_COSMOS]
        logger.info(f'table_labels_from_COSMOS: {table_labels_from_COSMOS}')
        # table detection lists for stats
        missing_refs = [x for x in table_references_from_text if x not in table_labels_from_COSMOS]
        detected_refs = list(set([x for x in table_labels_from_COSMOS if x in table_references_from_text]))
        detected_not_true = [x for x in table_labels_from_COSMOS if x not in detected_refs]

        # COSMOS table detection stats: precision, recall, f1
        try:
            tp = len(detected_refs)
        except(TypeError):
            tp = 0
        try:
            fn = len(missing_refs)
        except(TypeError):
            fn = 0
        try:
            fp = len(detected_not_true)
        except(TypeError):
            fp = 0
        try:
            precision = tp / (tp + fp)
        except(ZeroDivisionError):
            precision = 1
        try:
            recall = tp / (tp + fn)
        except(ZeroDivisionError):
            recall = 1
        try:
            f1_score = 2 * (precision * recall) / (precision + recall)
        except(ZeroDivisionError):
            f1_score = 1

        logger.info(f'tp:{tp}, fp:{fp}, fn:{fn}, pr={precision}, re={recall}, f1:{f1_score}')

        statistics_df = pd.DataFrame.from_dict({pdf_name: [precision, recall, f1_score]},
                                               orient='index',
                                               columns=['precision', 'recall', 'f1_score'])

        # get DataFrame of any actual tables not detected by COSMOS
        logger.info(f'missing_refs: {missing_refs}')
        if len(missing_refs) > 0:
            missing_refs = pd.DataFrame.from_dict({pdf_name: missing_refs}, orient='index')
        if len(missing_refs) == 0:
            missing_refs = None

    # collect the contexts here - for each row in the original_tables_df that has a table_label
    for index, row in original_tables_df.iterrows():
        table_label = row['table_label']
        if table_label:
            # collect contexts from doc_df
            contexts = []
            # for words in all_doc_words
            try:
                # mark where each label occurs in list of all doc words
                # allow for any non alphanum characters beside the table label e.g. Table 2. or Table 2,
                indices = [
                    j
                    for j in range(len(all_doc_words))
                    if re.match(r'^([^\w]?){0,}(' + table_label + r')([^\w]?){0,}$',
                                ' '.join(all_doc_words[j:j + label_length]).lower())
                ]

                logger.debug(f'indices: {indices}')

                # iterate over list in indices column to extract words before labels in content
                logger.debug('getting semantic context')
                prefixes = [
                    ' '.join(all_doc_words[i - spans:i])
                    if i - spans >= 0
                    else ' '.join(all_doc_words[0:i])
                    for i in indices
                ]

                logger.debug(f'prefixes: {prefixes}')
                # iterate over list in indices column to extract words after labels in content
                suffixes = [
                    ' '.join(all_doc_words[i + label_length:i + label_length + spans])
                    if i + label_length + spans <= len(all_doc_words)
                    else ' '.join(all_doc_words[i + label_length:len(all_doc_words)])
                    for i in indices
                ]

                logger.debug(f'suffixes: {suffixes}')

                for prefix, suffix in list(zip(prefixes, suffixes)):
                    new_context = ''.join(
                        prefix + ' ' + table_label + ' ' + suffix
                    )
                    if new_context:
                        contexts.append(new_context)
                        logger.debug(f'contexts: {contexts}')

            except Exception as e:
                logger.info(f"{pdf_name} failed fetching context indices for '{table_label}':\n {e}")
                logger.info(f'table row:\n {row}')

            # add all context to context column
            tables_df.at[index, context_column] = ' '.join(contexts).strip()

    return tables_df.loc[:, tables_df_columns], missing_refs, statistics_df
