from Parser.parse_html_to_postgres import parse_html_to_postgres

if __name__ == '__main__':
    input_folder = '/home/paulluh/Cosmos/exp/data4parser/html/files/'

    # intermediate folder location (will be auto-generated)
    merge_folder = '/home/paulluh/Cosmos/exp/data4parser/html/merged/'
    output_html = '/home/paulluh/Cosmos/exp/data4parser_out/html/'
    output_words = '/home/paulluh/Cosmos/exp/data4parser_out/words/'

    db_connect_str = 'postgres://postgres:password@localhost:5432/cosmos7'

    strip_tags = ['strong', 'em']
    ignored_file_when_link = []

    parse_html_to_postgres(input_folder, output_html, merge_folder, output_html, db_connect_str, ignored_file_when_link)
