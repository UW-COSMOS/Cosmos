from parse_html_to_postgres import parse_html_to_postgres


ingestion_settings = {
    'input_folder'           : '/home/vangle/UnicodeParser/html/files/',
    'merge_folder'           : '/home/vangle/UnicodeParser/html/merged/',
    'output_html'            : '/home/vangle/UnicodeParser/output/html/',
    'output_words'           : '/home/vangle/UnicodeParser/output/words/',
    'output_equations'       : '/home/vangle/UnicodeParser/output/equations/',
    'db_connect_str'         : 'postgres://postgres:vangle@localhost:5432/cosmos16',
    'strip_tags'             : ['strong', 'em'],
    'ignored_file_when_link' : [],
}

# Parse html files to postgres db
input_folder = ingestion_settings['input_folder']

# intermediate folder location (will be auto-generated)
merge_folder = ingestion_settings['merge_folder']
output_html = ingestion_settings['output_html']
output_words = ingestion_settings['output_words']
output_equations = ingestion_settings['output_equations']

db_connect_str = ingestion_settings['db_connect_str']

strip_tags = ingestion_settings['strip_tags']
ignored_file_when_link = ingestion_settings['ignored_file_when_link']

parse_html_to_postgres(input_folder, output_html, merge_folder, output_words, output_equations, db_connect_str, strip_tags, ignored_file_when_link, 'output.csv', '/home/vangle/corenlp/stanford-corenlp-full-2018-10-05',store_into_postgres=True)

