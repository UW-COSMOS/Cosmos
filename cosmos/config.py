import os


#base = '/app'
output = '/output'

# weight for im2latex

IM2LATEX_WEIGHT = '/app/im2latex_weights_prod/'


## POSTGRES INGESTION FILE SETTINGS
ingestion_settings = {
    'input_folder'           : os.path.join(output, 'html'),
    'merge_folder'           : os.path.join(output, 'html', 'merged'),
    'output_html'            : os.path.join(output, 'html_out', 'html/'),
    'output_words'           : os.path.join(output, 'html_out', 'words/'),
	'output_equations'       : os.path.join(output, 'html_out', 'equations/'),
    'db_connect_str'         : 'postgres://postgres:@cosmos_postgres:5432/cosmos',
    'db_template_str'        : 'postgres://postgres:@cosmos_postgres:5432/template1',
    'strip_tags'             : ['strong', 'em'],
    'ignored_file_when_link' : [],
}

