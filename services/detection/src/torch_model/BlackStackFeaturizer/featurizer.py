'''
Featurizer utilizing the classifier created by blackstack
'''

from bs4 import BeautifulSoup
import helpers
import heuristics
import os
import math
import re
import numpy as np


def area_summary_offset(area):
    summary = {}
    summary.update(area)
    coords = area.get('data-coordinates')
    spl = coords.split(' ')
    origin_x1 = int(spl[0])
    origin_y1 = int(spl[1])


    # Number of lines
    summary['lines'] = len(summary['soup'].find_all('span', 'ocr_line'))
    summary['line_heights'] = []

    for line in summary['soup'].find_all('span', 'ocr_line'):
        bbox = helpers.extractbbox(line.get('title'))
        height = bbox['y2'] - bbox['y1']
        summary['line_heights'].append(height)

    # Number of words
    try:
        summary['words'] = len(list(filter(None, summary['soup'].getText().strip().replace('\n', ' ').replace('  ', ' ').split(' '))))
    except:
        summary['words'] = 0

    # Area
    summary['area'] = (summary['x2'] - summary['x1']) * (summary['y2'] - summary['y1'])

    # Get spacing of words
    summary['x_gaps'] = np.zeros(summary['x2'] - summary['x1'], dtype=np.int)

    # Words per line
    summary['words_in_line'] = []
    summary['word_distances'] = []
    summary['word_heights'] = []
    summary['word_areas'] = []
    summary['words_per_line'] = []

    # Record the x position of the first word in each line
    summary['first_word_x'] = []

    # Iterate on each line in the area
    for line in summary['soup'].find_all('span', 'ocr_line'):
        # For each line, get words
        words = line.find_all('span', 'ocrx_word')

        # Record the number of words in this line
        summary['words_per_line'].append(len(words))

        for word_idx, word in enumerate(words):
            wordbbox = helpers.extractbbox(word.get('title'))

            word_area = (wordbbox['x2'] - wordbbox['x1']) * (wordbbox['y2'] - wordbbox['y1'])
            if word_area > summary['area'] or \
                    wordbbox['x2'] > summary['x2'] or \
                    wordbbox['x1'] < summary['x1'] or \
                    wordbbox['y1'] < summary['y1'] or \
                    wordbbox['y2'] > summary['y2']:
                print("Word outside of the enclosing area! Tesseract's black box strikes again!")
                continue

            # Record the x coordinate of the first word of each line
            if word_idx == 0:
                summary['first_word_x'] = wordbbox['x1'] + coords[0]

            summary['word_heights'].append(wordbbox['y2'] - wordbbox['y1'])
            summary['word_areas'].append(word_area)

            for x in range(wordbbox['x1'] - summary['x1'], wordbbox['x2'] - summary['x1']):
                summary['x_gaps'][x] = 1

            # If word isn't the last word in a line, get distance between word and word + 1
            if word_idx != (len(words) - 1):
                wordP1bbox = helpers.extractbbox(words[ word_idx + 1 ].get('title'))
                # Pythagorean theorum FTW
                summary['word_distances'].append(math.sqrt(math.pow((wordP1bbox['x1'] - wordbbox['x2']), 2) + math.pow((wordP1bbox['y1'] - wordbbox['y1']), 2)))

    # Count whitespace gaps
    summary['gaps'] = helpers.get_gaps(summary['x_gaps'])

    # Get the mean of the differences of the word distances (all the same == 0, difference increases away from 0)
    summary['word_separation_index'] = 0 if summary['words'] == 0 else helpers.meanOfDifferences(summary['word_distances'])

    # Quantify the variation in the height of words in this area
    summary['word_height_index'] = 0 if summary['words'] == 0 else helpers.meanOfDifferences(summary['word_heights'])

    # Get the average word height of this area
    summary['word_height_avg'] = 0 if summary['words'] == 0 else np.nanmean(summary['word_heights'])

    # Get word/area ratio
    summary['word_area_index'] = 0 if summary['words'] == 0 else np.sum(summary['word_areas']) / float(summary['area'])

    return summary


def get_features(pdf_html_dir):
    for pdf_html_page in os.listdir(pdf_html_dir):
        path = os.path.join(pdf_html_dir, pdf_html_page)
        pages = []
        with open(path, 'r') as rf:
            soup = BeautifulSoup(rf, 'html.parser')
            tstring = soup.title.string
            # Example title: S1470160X05000063.pdf-0000
            page_number_re = re.match(r'.*-(\d{4})')
            page_number = page_number_re.group()
            hocr_areas = soup.find_all('div', 'hocr')
            coords =
            pages.append({
                'page_no': page_number,
                'soup': soup,
                'page': tstring,
                'areas': [helpers.area_summary(area) for area in merged_areas],
                'lines': [line for line in soup.find_all('span', 'ocr_line')]
            })
        page_areas = [page['areas'] for page in pages]
        doc_stats = helpers.summarize_document([area for areas in page_areas for area in areas])




