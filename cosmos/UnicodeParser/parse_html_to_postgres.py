import os, sys
import shutil
sys.path.append(os.path.dirname(__file__))

from link import link
from pagemerger import pagemerger
from parse import parse
from parse_preprocess import preprocess
from insert_equation import insert_equation_tuple
from table_X import build_table_X
from csv_generator import generate_csv
from table_one import var_in_text



def parse_html_to_postgres(input_folder, output_html, merge_folder, output_words, output_equations, db_connect_str,
                           strip_tags, ignored_file_when_link, csv_file, store_into_postgres=True):
    assert os.path.isabs(input_folder)
    assert os.path.isabs(output_html)
    assert os.path.isabs(merge_folder)
    assert os.path.isabs(output_words)

    """
    # 1. group files by file name
    merge.stamp: pagemerger.py
        rm -r -f $(merge_folder)
        mkdir -p $(merge_folder)
        python pagemerger.py --rawfolder $(input_folder) --outputfolder $(merge_folder)
        @touch merge.stamp
    """
    if os.path.exists(merge_folder):
        shutil.rmtree(merge_folder)
    os.makedirs(merge_folder, exist_ok=True)
    pagemerger(input_folder, merge_folder)

    """
    # 2. preprocess the input html and store intermediate json and html in the output folder declared above.
    preprocess.stamp: preprocess.py merge.stamp
        rm -r -f $(output_html)
        rm -r -f $(output_words)
        mkdir -p $(output_html)
        mkdir -p $(output_words)
        @$(foreach file,$(all_inputs),\
        python preprocess.py --input $(merge_folder)$(file) --output_words $(output_words)$(file).json --output_html $(output_html)$(file);)
        @touch preprocess.stamp
    """
    if os.path.exists(output_html):
        shutil.rmtree(output_html)
    if os.path.exists(output_words):
        shutil.rmtree(output_words)
    if os.path.exists(output_equations):
        shutil.rmtree(output_equations)

    os.makedirs(output_html, exist_ok=True)
    os.makedirs(output_words, exist_ok=True)
    os.makedirs(output_equations, exist_ok=True)

    all_inputs = [f for f in os.listdir(merge_folder)]
    for html_file in all_inputs:
        preprocess(os.path.join(merge_folder, html_file), "%s.json" % (os.path.join(output_words, html_file)),
                   os.path.join(output_html, html_file), "%s.json" % (os.path.join(output_equations, html_file)), strip_tags)

    if store_into_postgres:
        """
        # 3. run the fonduer parser on the generated html file. This will fill in the postgres dabase with everything
        # fonduer can understand except the coordinate information.
        parse.stamp: preprocess.stamp parse.py
        python parse.py --html_location $(output_html) --database $(db_connect_str)
        @touch parse.stamp
        """
        parse(output_html, db_connect_str)

        """
        # 4. run the link file to insert coordinate information into fonduer based on the information from the json output folder (aka. hocr)
        link.stamp: parse.stamp link.py
            python link.py --words_location $(output_words) --database $(db_connect_str)
            @touch link.stamp
        """
        link(output_words, db_connect_str, ignored_file_when_link)

        insert_equation_tuple(db_connect_str, output_equations)

        var_in_text(db_connect_str)

        build_table_X(db_connect_str,'/app/stanford-corenlp-full-2018-10-05')

        generate_csv(db_connect_str, csv_file)
        
