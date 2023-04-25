import os
import sys
from dataclasses import dataclass
from pathlib import Path
import pandas as pd
import camelot
import pdfplumber

from typing import List, Tuple, Union, Optional
import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.WARNING)


COSMOS_SCALE_HEIGHT = 1920


class DocHeightNotSetError(Exception):
    pass


class DocWidthNotSetError(Exception):
    pass


@dataclass
class TableLocation:
    """
    Extract a table from a PDF using one row of a COSMOS _tables.parquet
    COSMOS parquet coordinate origin: 0,0 origin top left, table area defined from top left corner to bottom right
    camelot coordinate origin: bottom left, table area defined from bottom left corner to top right
    """
    x1: float = 0
    y1: float = 0
    x2: float = 0
    y2: float = 0
    doc_width: float = 0
    doc_height: float = 0
    page: int = 0
    pdf_path: str = ''
    cosmos_png_path: str = ''
    pkl_path: Optional[str] = None

    def __post_init__(self):
        if not self.doc_width or self.doc_width == 0:
            raise DocWidthNotSetError(f"pdf: {self.pdf_path};"
                                      f"x1,y1,x2,y2: {self.x1},{self.y1},{self.x2},{self.y2}; page: {self.page}")

        if not self.doc_height or self.doc_height == 0:
            raise DocHeightNotSetError(f"pdf: {self.pdf_path};"
                                       f"x1,y1,x2,y2: {self.x1},{self.y1},{self.x2},{self.y2}; page: {self.page}")

        self.scale_factor = COSMOS_SCALE_HEIGHT / self.doc_height

        if self.x2 / self.scale_factor > self.doc_width:  # can't compare COSMOS parquet doc width and height!
            self.landscape = True
        else:
            self.landscape = False

        if self.landscape:
            self.doc_height, self.doc_width = self.doc_width, self.doc_height

    @property
    def pdf_name(self):
        return Path(self.pdf_path).name

    @property
    def cosmos_page_png(self):
        """tmp png filepath as Path"""
        png_path = Path(self.cosmos_png_path)
        png_name = self.pdf_name + '_' + self.camelot_page + '.png'
        return png_path / png_name

    @property
    def camelot_list(self) -> List[int]:
        """
        scale, flip y axis, return in camelot order [x1, y1, x2, y2]

        camelot coord _system_ origin is bottom left, but table area is
        defined from top left to bottom right.
        COSMOS page coordinates are scaled by a factor to make the longest dimension = 1920 px
        """

        camelot_x1 = self.x1 / self.scale_factor
        camelot_x2 = self.x2 / self.scale_factor

        camelot_y1 = self.doc_height - (self.y1 / self.scale_factor)
        camelot_y2 = self.doc_height - (self.y2 / self.scale_factor)

        # if negative y co-ords - set to pdf page extents
        if (camelot_y1 < 0) or (camelot_y2 < 0):
            logging.warning(f'camelot co-ords with negative y: {self.pdf_name}')
            camelot_y1 = 0
            camelot_y2 = self.doc_height

        coords = [camelot_x1, camelot_y1, camelot_x2, camelot_y2]

        return [int(x) for x in coords]

    @property
    def camelot_table_area(self) -> List[str]:
        """table area coordinates in camelot order formatted as a string in a list"""
        return [','.join([str(x) for x in self.camelot_list])]

    @property
    def camelot_page(self) -> str:
        """get page number as a str"""
        return str(self.page)

    @property
    def pdfplumber_table_area(self) -> List[int]:
        """
        scale and return coordinates in pdfplumber order (x1, y1, x2, y2) as an integer tuple 
        pdfplumber coord _system_ origin is top left, which is the same as COSMOS
        pdfplumber table area is defined from top left to bottom right.
        COSMOS page coordinates are scaled by a factor to make the longest dimension = 1920 px
        """
        pdfplumber_x1 = self.x1 / self.scale_factor
        pdfplumber_x2 = self.x2 / self.scale_factor
        pdfplumber_y1 = self.y1 / self.scale_factor
        pdfplumber_y2 = self.y2 / self.scale_factor
        coords = (pdfplumber_x1, pdfplumber_y1, pdfplumber_x2, pdfplumber_y2)
        return tuple(map(int, coords))
    
    @property
    def pdfplumber_page(self) -> str:
        """get page number as a str"""
        return str(self.page - 1)

    def extract_table(self) -> Tuple[Optional[pd.DataFrame],
                                     str,
                                     Optional[dict],
                                     Optional[camelot.core.TableList]]:
        """run camelot extraction on pdf table area"""
        camelot_success = False
        pdfplumber_success = False
        camelot_df, pdfplumber_df, report, table = None, None, None, None
        log = ''

        try:
            table = camelot.read_pdf(self.pdf_path,
                                     pages=self.camelot_page,
                                     table_areas=self.camelot_table_area,
                                     flavor='stream'
                                     )
            camelot_df = table[0].df
            report = table[0].parsing_report
            camelot_success = True
            log += 'Camelot table extracted\n'
        except Exception as e:
            log += f'Camelot extract failed: {e}\n{self.pdf_path}\n{self.camelot_page}\n{self.camelot_table_area}\n'

        try:
            with pdfplumber.open(self.pdf_path) as pdf:
                page = pdf.pages[int(self.pdfplumber_page)]
                table = page.crop(self.pdfplumber_table_area, relative=False, strict=True).extract_table(table_settings={'vertical_strategy':'text'})
                pdfplumber_df = pd.DataFrame(table[1:], columns=table[0])
                pdfplumber_success = True
                log += 'Pdfplumber table extracted\n'
        except Exception as e:
            log += f'Pdfplumber extract failed: {e}\n{self.pdf_path}\n{self.pdfplumber_page}\n{self.pdfplumber_table_area}\n'

        if camelot_success or pdfplumber_success:
            return [camelot_df, pdfplumber_df], log, report, table
        else:
            logging.error(log)
            self.pkl_path = None
            return None, log, None, None


class TableLocationProcessor:
    """extract tables to df from a whole _tables.parquet"""

    def __init__(self, tables_parquet: Union[pd.DataFrame, Path, str],
                 pdf_path: str,
                 png_path: str,
                 output_path: str,
                 d_score_cutoff: float = -10,
                 pp_score_cutoff: float = 0,
                 flavor: str = 'stream') -> None:
        """
        TableLocationProcessor takes a COSMOS _tables.parquet and pdf_path as input, then with extract_df_pickles()
        produces a directory of extracted tables as pickled pandas dataframes and returns a new dataframe containing
        paths to each extracted dataframe pickle file.

        :param tables_parquet: dataframe, or str of path to .parquet, to load as dataframe
        :param pdf_path: str path to dir of original pdfs (COSMOS env config)
        :param png_path: str path to dir of intermediate COSMOS generated pngs of detected tables (COSMOS env config)
        :param d_score_cutoff: detection score cutoff to not examine table rows under, default, no cutoff: -10
        :param pp_score_cutoff: post_process score cutoff to not examine table rows under, default, no cutoff: 0
        :param flavor: camelot table detection method 'stream' or 'lattice', default: 'stream'
        :return: None
        """

        try:
            tables_parquet = Path(tables_parquet)
            self.df = pd.read_parquet(tables_parquet)
            self.path = tables_parquet
        except TypeError:
            self.df = tables_parquet
            self.path = None

        self.png_path = png_path
        self.output_path = output_path
        if not os.path.exists(self.output_path):
            os.makedirs(self.output_path)

        self.orig_df = self.df.copy(deep=True)
        if len(self.df) == 0:
            print(f"No tables found in {tables_parquet}")
            sys.exit(0)
        self.df = self.df[(self.df.detect_score >= d_score_cutoff) &
                          (self.df.postprocess_score >= pp_score_cutoff)]
        self.table_count = len(self.df)
        self.pdf_path = pdf_path
        self.flavor = flavor

    def _add_pkl_paths_column(self) -> None:
        """add pkl_path to the COSMOS _tables.parquet"""
        self.df = pd.concat([self._create_pkl_path_per_doc_df(x)
                             for x in self._get_individual_doc_dfs(self.df)])

    @staticmethod
    def _get_individual_doc_dfs(parquet_df: pd.DataFrame) -> List[pd.DataFrame]:
        """return a list of dfs, each df only has all rows for one pdf_name"""
        every_unique_pdf_name = parquet_df.pdf_name.unique()
        return [parquet_df[parquet_df['pdf_name'] == unique_pdf_name]
                for unique_pdf_name in every_unique_pdf_name]

    def _create_pkl_path_per_doc_df(self, doc_df: pd.DataFrame) -> pd.DataFrame:
        """add pkl_path to every row in a doc_df"""
        doc_df.insert(loc=0,
                      column='row_num',
                      value=[str(x) for x in range(len(doc_df))])

        doc_df = doc_df.apply(self._create_pkl_path, axis=1)

        doc_df.drop('row_num', axis=1, inplace=True)
        return doc_df

    def _create_pkl_path(self, row: pd.Series) -> pd.Series:
        """define pkl_path per row, call from pandas apply in _create_pkl_path_per_doc_df"""
        row['pkl_path'] = self.output_path + row['pdf_name'][:-4] + '_' + row['row_num'] + '.pkl'
        return row

    def _get_table_locations(self) -> List[TableLocation]:
        """convert each _table.parquet dataframe row into a TableLocation"""
        return [
            TableLocation(x1=row['obj_bbs'][0],
                          y1=row['obj_bbs'][1],
                          x2=row['obj_bbs'][2],
                          y2=row['obj_bbs'][3],
                          doc_width=row['pdf_dims'][2],
                          doc_height=row['pdf_dims'][3],
                          page=row['obj_page'],
                          pdf_path=self.pdf_path + row['pdf_name'],
                          cosmos_png_path=self.png_path,
                          pkl_path=row['pkl_path'])
            for i, row in self.df.iterrows()]

    def _extract_tables(self) -> List[Tuple[str, pd.DataFrame]]:
        """get pkl_path output file path and dataframe from each table location"""
        self._add_pkl_paths_column()
        for x in self._get_table_locations():
            print(f"Working on {x.pdf_path}")
            yield (x.pkl_path, x.extract_table()[0])

    def extract_csvs(self) -> None:
        """save each table location df as a csv with that pdfs name and an incrementing int,
        the table extracted by pdfplumber ends with the '_pdfplumber.csv' extension """
        for pkl_path, df in self._extract_tables():
            csv_path = pkl_path[:-4] + '.csv'
            csv_path_pdfplumber = pkl_path[:-4] + '_pdfplumber.csv'
            try:
                df[0].to_csv(csv_path, index=False)
            except (AttributeError, TypeError):
                logging.info(f'no df to csv: {pkl_path}')
            try:
                df[1].to_csv(csv_path_pdfplumber, index=False)
            except (AttributeError, TypeError):
                logging.info(f'no df to csv: {csv_path_pdfplumber}')

    def extract_pickles(self) -> None:
        """save each table location df as a pickled df with that pdf's name and an incrementing int
        AND update the _tables.parquet with the path to those pkl files per row,
        the table extracted by pdfplumber ends with the '_pdfplumber.pkl' extension"""
        for pkl_path, df in self._extract_tables():
            try:
                df[0].to_pickle(pkl_path)
            except (AttributeError, TypeError):
                logging.info(f'no table df to pickle: {pkl_path}')
            try:
                df[1].to_pickle(pkl_path[:-4] + '_pdfplumber.pkl')
            except (AttributeError, TypeError):
                logging.info(f"no table df to pickle: {pkl_path[:-4] + '_pdfplumber.pkl'}")   

        _ = self._update_table_parquet()

    def _update_table_parquet(self) -> pd.DataFrame:
        """update the _tables.parquet with the path to those pkl files per row and return that df """
        merged_df = pd.concat([self.orig_df, self.df])
        merged_df = merged_df[~merged_df.index.duplicated(keep='last')].sort_index()
        if self.path:
            logger.info('updating that original parquet!')
            merged_df.to_parquet(self.path)
        return merged_df
