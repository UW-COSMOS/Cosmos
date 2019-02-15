from Parser.parse_html_to_postgres import parse_html_to_postgres
from config import ingestion_settings as settings

if __name__ == '__main__':
    input_folder = settings['input_folder']

    # intermediate folder location (will be auto-generated)
    merge_folder = settings['merge_folder']
    output_html = settings['output_html']
    output_words = settings['output_words']

    db_connect_str = settings['db_connect_str']

    strip_tags = settings['strip_tags']
    ignored_file_when_link = settings['ignored_file_when_link']

    parse_html_to_postgres(input_folder, output_html, merge_folder, output_html, db_connect_str, strip_tags, ignored_file_when_link)
