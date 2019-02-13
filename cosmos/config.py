from mrcnn.config import Config
import os

class PageConfig(Config):
	BACKBONE = "resnet50"
	NAME = "pages_uncollapsed"
	GPU_COUNT = 1
	IMAGES_PER_GPU = 2
	NUM_CLASSES = 16
	IMAGE_MIN_DIM = 1920
	IMAGE_MAX_DIM = 1920
	RPN_ANCHOR_SCALES = (32,64,256, 512, 1024)
	TRAIN_ROIS_PER_IMAGE = 100
	LEARNING_RATE = 0.00005
	MAX_GT_INSTANCES = 15
	USE_MINI_MASK = True


base = '/vol'
## POSTGRES INGESTION FILE SETTINGS
ingestion_settings = {
    'input_folder'           : os.path.join(base, 'html'),
    'merge_folder'           : os.path.join(base, 'html', 'merged'),
    'output_html'            : os.path.join(base, 'html_out', 'html/'),
    'output_words'           : os.path.join(base, 'html_out', 'words/'),
    'db_connect_str'         : 'postgres://postgres:password@localhost:5432/cosmos',
    'strip_tags'             : ['strong', 'em'],
    'ignored_file_when_link' : [],
}

