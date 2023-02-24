import pytest
from pathlib import Path
import shutil
import pandas as pd
from table_extraction import TableLocationProcessor, TableLocation, \
    DocHeightNotSetError, DocWidthNotSetError


@pytest.fixture(scope='module')
def artifacts_dir(tmpdir_factory) -> Path:
    """create tmp dir with .parquet, .pdf, and .png"""
    p = tmpdir_factory.mktemp('mock_cosmos_output/')
    return p


@pytest.fixture
def copy_files_to_artifacts_dir(artifacts_dir: Path) -> Path:
    files_to_copy = list(Path('./table_extraction_test_artifacts').glob('mockup*'))
    for file in files_to_copy:
        shutil.copy(file, artifacts_dir / file.name)
    return artifacts_dir


@pytest.fixture
def add_cosmos_table_parquet_to_dir(artifacts_dir: Path) -> Path:
    """create tmp dir with .parquet, .pdf, and .png"""
    fake_parquet = dict(pdf_name='mockup.pdf',
                        dataset_id='mockups',
                        caption_content='',
                        caption_page='',
                        caption_bb='',
                        pdf_dims=[[0, 0, 611, 791]],
                        detect_score=1.05,
                        postprocess_score=0.90,
                        content='',
                        obj_page=1,
                        obj_bbs=[[177, 364, 1310, 725]],
                        img_path=str(artifacts_dir / 'mockup.png')
                        )
    pd.DataFrame.from_dict(fake_parquet).to_parquet(artifacts_dir / 'mockup_tables.parquet')
    return artifacts_dir


@pytest.fixture
def add_cosmos_table_parquet_to_dir_landscape(artifacts_dir: Path) -> Path:
    """create tmp dir with .parquet, .pdf, and .png"""

    fake_parquet = dict(pdf_name='mockup_landscape.pdf',
                        dataset_id='mockups',
                        caption_content='',
                        caption_page='',
                        caption_bb='',
                        pdf_dims=[[0, 0, 611, 791]],  # COSMOS landscape page extents are captured as portrait
                        detect_score=1.05,
                        postprocess_score=0.90,
                        content='',
                        obj_page=1,
                        obj_bbs=[[393, 381, 1530, 544]],
                        img_path=str(artifacts_dir / 'mockup_landscape.png')
                        )
    pd.DataFrame.from_dict(fake_parquet).to_parquet(artifacts_dir / 'mockup_tables_landscape.parquet')
    return artifacts_dir


@pytest.fixture(autouse=True)
def dir_with_cosmos_artifacts(artifacts_dir, copy_files_to_artifacts_dir, add_cosmos_table_parquet_to_dir,
                              add_cosmos_table_parquet_to_dir_landscape) -> Path:
    return artifacts_dir


@pytest.fixture
def mock_tables_parquet(artifacts_dir) -> Path:
    return artifacts_dir / 'mockup_tables.parquet'


@pytest.fixture
def mock_tables_parquet_landscape(artifacts_dir) -> Path:
    return artifacts_dir / 'mockup_tables_landscape.parquet'


@pytest.fixture
def mock_pdf_document(artifacts_dir) -> Path:
    return artifacts_dir / 'mockup.pdf'


@pytest.fixture
def mock_pdf_document_landscape(artifacts_dir) -> Path:
    return artifacts_dir / 'mockup_landscape.pdf'


@pytest.fixture
def mock_table_png(artifacts_dir) -> Path:
    return artifacts_dir / 'mockup.png'


@pytest.fixture
def pre_made_table_location(mock_tables_parquet, mock_table_png, mock_pdf_document):
    df = pd.read_parquet(mock_tables_parquet)
    tl = TableLocation(x1=df.loc[0, 'obj_bbs'][0],
                       y1=df.loc[0, 'obj_bbs'][1],
                       x2=df.loc[0, 'obj_bbs'][2],
                       y2=df.loc[0, 'obj_bbs'][3],
                       doc_width=df.loc[0, 'pdf_dims'][2],
                       doc_height=df.loc[0, 'pdf_dims'][3],
                       page=df.loc[0, 'obj_page'],
                       pdf_path=str(mock_pdf_document),
                       cosmos_png_path=str(mock_table_png),
                       pkl_path=None
                       )
    return tl


@pytest.fixture
def pre_made_table_location_landscape(mock_tables_parquet_landscape, mock_table_png, mock_pdf_document_landscape):
    df = pd.read_parquet(mock_tables_parquet_landscape)
    tl = TableLocation(x1=df.loc[0, 'obj_bbs'][0],
                       y1=df.loc[0, 'obj_bbs'][1],
                       x2=df.loc[0, 'obj_bbs'][2],
                       y2=df.loc[0, 'obj_bbs'][3],
                       doc_width=df.loc[0, 'pdf_dims'][2],
                       doc_height=df.loc[0, 'pdf_dims'][3],
                       page=df.loc[0, 'obj_page'],
                       pdf_path=str(mock_pdf_document_landscape),
                       cosmos_png_path=str(mock_table_png),
                       pkl_path=None
                       )
    return tl


@pytest.fixture
def pre_made_table_location_processor(artifacts_dir, mock_tables_parquet, mock_table_png, mock_pdf_document):
    tlp = TableLocationProcessor(mock_tables_parquet,
                                 pdf_path=str(artifacts_dir)+'/',
                                 png_path=str(mock_table_png),
                                 output_path=str(artifacts_dir)+'/',
                                 pp_score_cutoff=0.72
                                 )
    return tlp


class TestTableLocation:
    def test_table_location_instantiates(self, mock_tables_parquet, mock_table_png, mock_pdf_document):
        df = pd.read_parquet(mock_tables_parquet)
        tl = TableLocation(x1=df.loc[0, 'obj_bbs'][0],
                           y1=df.loc[0, 'obj_bbs'][1],
                           x2=df.loc[0, 'obj_bbs'][2],
                           y2=df.loc[0, 'obj_bbs'][3],
                           doc_width=df.loc[0, 'pdf_dims'][2],
                           doc_height=df.loc[0, 'pdf_dims'][3],
                           page=df.loc[0, 'obj_page'],
                           pdf_path=str(mock_pdf_document),
                           cosmos_png_path=str(mock_table_png),
                           pkl_path=None
                           )

        assert tl.pdf_name == 'mockup.pdf'
        assert tl.x1 == 177
        assert tl.y1 == 364
        assert tl.x2 == 1310
        assert tl.y2 == 725
        assert tl.doc_width == 611
        assert tl.doc_height == 791
        assert tl.page == 1
        assert tl.pdf_path[-10:] == 'mockup.pdf'
        assert tl.cosmos_png_path[-10:] == 'mockup.png'

    def test_raise_doc_width_error(self, mock_tables_parquet, mock_table_png, mock_pdf_document):
        with pytest.raises(DocWidthNotSetError):
            df = pd.read_parquet(mock_tables_parquet)
            TableLocation(x1=df.loc[0, 'obj_bbs'][0],
                          y1=df.loc[0, 'obj_bbs'][1],
                          x2=df.loc[0, 'obj_bbs'][2],
                          y2=df.loc[0, 'obj_bbs'][3],
                          doc_width=0,
                          doc_height=df.loc[0, 'pdf_dims'][3],
                          page=df.loc[0, 'obj_page'],
                          pdf_path=str(mock_pdf_document),
                          cosmos_png_path=str(mock_table_png),
                          pkl_path=None
                          )

    def test_raise_doc_height_error(self, mock_tables_parquet, mock_table_png, mock_pdf_document):
        with pytest.raises(DocHeightNotSetError):
            df = pd.read_parquet(mock_tables_parquet)
            TableLocation(x1=df.loc[0, 'obj_bbs'][0],
                          y1=df.loc[0, 'obj_bbs'][1],
                          x2=df.loc[0, 'obj_bbs'][2],
                          y2=df.loc[0, 'obj_bbs'][3],
                          doc_width=df.loc[0, 'pdf_dims'][2],
                          doc_height=0,
                          page=df.loc[0, 'obj_page'],
                          pdf_path=str(mock_pdf_document),
                          cosmos_png_path=str(mock_table_png),
                          pkl_path=None
                          )

    def test_returns_camelot_coords(self, pre_made_table_location):
        assert pre_made_table_location.camelot_table_area == ['72,641,539,492']

    def test_returns_camelot_page(self, pre_made_table_location):
        assert pre_made_table_location.camelot_page == '1'

    def test_detects_orientation_portrait(self, pre_made_table_location):
        assert pre_made_table_location.landscape is False

    def test_detects_orientation_landscape(self, pre_made_table_location_landscape):
        assert pre_made_table_location_landscape.landscape is True

    def test_extract_table(self, pre_made_table_location):
        df, log, report, table = pre_made_table_location.extract_table()
        assert isinstance(df, pd.DataFrame)
        assert df.iloc[0, 1] == 'Apples'
        assert df.iloc[1, 1] == '10'

    def test_extract_table_landscape(self, pre_made_table_location_landscape):
        df, log, report, table = pre_made_table_location_landscape.extract_table()
        assert isinstance(df, pd.DataFrame)
        assert df.iloc[0, 1] == 'Apples'
        assert df.iloc[1, 1] == '10'

    def test_table_extract_failure(self, pre_made_table_location):
        pre_made_table_location.x1 = 1
        pre_made_table_location.y1 = 1
        pre_made_table_location.x2 = 2
        pre_made_table_location.y2 = 2
        df, log, report, table = pre_made_table_location.extract_table()
        assert df is None
        assert isinstance(log, str)
        assert report is None
        assert table is None

    def test_y_coordinate_out_of_page_bounds(self, pre_made_table_location):
        pre_made_table_location.y2 = 4000
        x1, y1, x2, y2 = pre_made_table_location.camelot_list
        assert y1 == 0
        assert y2 == pre_made_table_location.doc_height


class TestTableLocationProcessor:
    def test_table_location_processor_instantiates_from_path(self, artifacts_dir, mock_tables_parquet, mock_table_png,
                                                             mock_pdf_document):
        tlp = TableLocationProcessor(mock_tables_parquet,
                                     pdf_path=str(mock_pdf_document),
                                     png_path=str(mock_table_png),
                                     output_path=str(artifacts_dir),
                                     pp_score_cutoff=0.72
                                     )
        assert isinstance(tlp.df, pd.DataFrame)

    def test_table_location_processor_instantiates_from_df(self, artifacts_dir, mock_tables_parquet, mock_table_png,
                                                           mock_pdf_document):
        tlp = TableLocationProcessor(pd.read_parquet(mock_tables_parquet),
                                     pdf_path=str(mock_pdf_document),
                                     png_path=str(mock_table_png),
                                     output_path=str(artifacts_dir),
                                     pp_score_cutoff=0.72
                                     )
        assert isinstance(tlp.df, pd.DataFrame)

    def test_add_pkl_locations(self, artifacts_dir, pre_made_table_location_processor):
        pre_made_table_location_processor._add_pkl_paths_column()
        assert pre_made_table_location_processor.df['pkl_path'][0] == str(Path(artifacts_dir) / 'mockup_0.pkl')

    def test_get_table_locations(self, pre_made_table_location_processor):
        pre_made_table_location_processor._add_pkl_paths_column()
        tll = pre_made_table_location_processor._get_table_locations()
        assert isinstance(tll, list)
        assert isinstance(tll[0], TableLocation)

    def test_extract_pkls(self, pre_made_table_location_processor):
        pre_made_table_location_processor.extract_pickles()
        pkl_file = Path(pre_made_table_location_processor.df['pkl_path'][0])
        unpickled = pd.read_pickle(pkl_file)
        assert pkl_file.exists()
        assert isinstance(unpickled, pd.DataFrame)
        assert unpickled.iloc[0, 1] == 'Apples'
        assert unpickled.iloc[1, 1] == '10'

    def test_extract_csvs(self, pre_made_table_location_processor):
        pre_made_table_location_processor.extract_csvs()
        csv_file = Path(pre_made_table_location_processor.df['pkl_path'][0][:-4]+'.csv')
        csv_df = pd.read_csv(csv_file)
        assert csv_file.exists()
        assert isinstance(csv_df, pd.DataFrame)
        assert csv_df.iloc[0, 1] == 'Apples'
        assert csv_df.iloc[1, 1] == '10'
