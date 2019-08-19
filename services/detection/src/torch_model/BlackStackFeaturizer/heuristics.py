"""

Copyright (c) 2018 John J Czaplewski

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"""
import helpers
import re
import numpy as np

'''
Possible types:
    header/footer
        - has_words
        - is_top_or_bottom
        - small_text?
        - n_lines <= 3
    body
        - has_words
        - normal_word_separation
        - normal_word_coverage
        - !overlaps
        - !small_text
        - !very_separated_words
        - !mostly_blank
        - !little_word_coverage
        - proportion_alpha > 0.7
        - !offset_words
        - n_lines > 1
        - !is_line
    graphic
        - mostly_blank
        - gaps!
        - little_word_coverage
        - overlaps?
        - n_lines > 1
        - !is_line
    graphic_caption
        - has_words
        - small_text?
        - overlaps?
        - normal_word_coverage
        - proportion_alpha > 0.7
        - !is_line
        -
    reference
        - has_words
        - small_text?
        - small_leading?
        - normal_word_coverage
        - proportion_alpha > 0.5...????
        - offset_words?
        - n_lines > 1
        - !is_line
    other
        - doesn't match other criteria
'''
# Does the area have any words?
def has_words(area):
    return True if area['words'] > 0 else False

# Does this area intersect a line
def line_intersect(area, all_areas):
    for line in [ area for area in all_areas if is_line(area)]:
        if helpers.rectangles_intersect(area, line):
            return True

    return False

# Is the text contained in this area smaller than "normal" (mean word height is +/- 0.25 of the document avg)
def small_text(area, doc_stats):
    if area['word_height_avg'] < (doc_stats['word_height_avg'] - (doc_stats['word_height_avg_std']/4)):
        return True
    else:
        return False

# Is the line height smaller than "normal" (mean leading is +/- 0.5 of the document avg)?
def small_leading(area, doc_stats):
    mean_area_leading = np.nanmean(area['line_heights'])

    if mean_area_leading >= doc_stats['line_height_avg'] - (doc_stats['line_height_std']/2) and mean_area_leading <= doc_stats['line_height_avg'] + (doc_stats['line_height_std']/2):
        return True
    else:
        return False

# Is this area only one line and contain no text?
#
# Separator lines
#    1 line
#    0 words
#    word separation index === 0
#    word height index === 0
#    word height average === 0
def is_line(area):
    if area['lines'] == 1 and area['words'] == 0 and area['word_separation_index'] == 0 and area['word_height_index'] == 0 and area['word_height_avg'] == 0:
        return True
    else:
        return False

# Is the area the first or last area (in y space) on the page?
def is_top_or_bottom(area, page_areas):
    min_y = min([ a['y1'] for a in page_areas ])
    max_y = max([ a['y1'] for a in page_areas ])

    if (area['y1'] <= min_y + 10 and area['y1'] >= min_y - 10) or (area['y1'] <= max_y + 10 and area['y1'] >= max_y - 10):
        return True
    else:
        return False

# Does the area contain much more white space than other areas?
#
# Giant blank areas are *probably* tables
#    average line height > (document average line height + 100)
#    area > 250000
def mostly_blank(area, doc_stats):
    if np.nanmean(area['line_heights']) > doc_stats['line_height_avg'] + 100 and area['area'] > 250000:
        return True
    else:
        return False


# Tables
#   very_separated_words == True
#   little_word_coverage == True
#   n_lines > 1
#   mostly_blank
#   overlaps

# is the separation of words in the area much greater than others?
# word separation index >= (document median word separation index + 1 standard deviation)
def very_separated_words(area, doc_stats):
    if (area['word_separation_index'] >= (doc_stats['word_separation_index_median'] + doc_stats['word_separation_index_std'])):
        return True
    else:
        return False

#  area covered by words <= (document word area median - 1 standard deviation)
def little_word_coverage(area, doc_stats):
    if (area['word_area_index'] <= (doc_stats['word_area_index_median'] - doc_stats['word_area_index_std'])):
        return False
    else:
        return True

# Is the separation of words in the area "normal"?
def normal_word_separation(area, doc_stats):
    if (area['word_separation_index'] < (doc_stats['word_separation_index_median'] + doc_stats['word_separation_index_std'])):
        return True
    else:
        return False

def normal_word_coverage(area, doc_stats):
    if (area['word_area_index'] > (doc_stats['word_area_index_median'] - (doc_stats['word_area_index_std']/float(2))) and area['word_area_index'] < (doc_stats['word_area_index_median'] + (doc_stats['word_area_index_std']/float(2)))):
        return True
    else:
        return False


def best_caption(area):
    lines = area['soup'].find_all('span', 'ocr_line')
    if len(lines) > 0:
        clean_line = lines[0].getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
        matches = re.match('(table|figure|fig|map|appendix|app|appx|tbl)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
        if matches is not None:
            return True

    return False

def good_caption(area):
    lines = area['soup'].find_all('span', 'ocr_line')
    if len(lines) > 0:
        clean_line = lines[0].getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
        matches = re.findall('(table|figure|fig|map|appendix|app|appx|tbl)(?:\.)? (?:(\d+\w+(?:\.)?)|(\d+))', clean_line, flags=re.IGNORECASE|re.MULTILINE)
        if len(matches):
            return True


    return False

def ok_caption(area):
    lines = area['soup'].find_all('span', 'ocr_line')
    for line in lines:
        clean_line = line.getText().strip().replace('\n', ' ').replace('  ', ' ').lower()
        matches = re.match('(\b\w{1,6}) \d+', clean_line, flags=re.IGNORECASE|re.MULTILINE)
        if matches is not None and helpers.similar_to_keyword(matches.groups()[0]):
            return True

    return False

# Does the area intersect with any other areas on the page?
def overlap(area, all_areas):
    for each in all_areas:
        if helpers.rectangles_intersect(area, each) and each['x1'] != area['x1'] and each['y1'] != area['y1'] and each['x2'] != area['x2'] and each['y2'] != area['y2']:
            return True

    return False

# For a given area, what proportion of the characters are [a-z]
def proportion_alpha(area):
    area_words = ' '.join(filter(None, area['soup'].getText().strip().replace('\n', ' ').replace('  ', ' ').split(' ')))
    # return the number of alpha characters divided by the total number of characters
    total_words = len(area_words.replace(' ', ''))
    if total_words == 0:
        return 0

    return len(re.findall('[a-zA-Z]', area_words)) / float(total_words)

# Is there a high standard deviation in the x position of the first word of each line?
def offset_words(area):
    # Record the x position of the first word of each line
    first_word_xs = []
    lines = area['soup'].find_all('span', 'ocr_line')
    for line in lines:
        words = line.find_all('span', 'ocrx_word')
        if len(words) > 0:
            coords = helpers.extractbbox(words[0].get('title'))
            first_word_xs.append(coords['x1'])

    if np.nanstd(first_word_xs) > 5:
        return True
    else:
        return False

def classify(area, doc_stats, all_areas):
    return {
        'has_words': has_words(area),
        'line_intersect': line_intersect(area, all_areas),
        'small_text': small_text(area, doc_stats),
        'small_leading': small_leading(area, doc_stats),
        'is_line': is_line(area),
        'is_top_or_bottom': is_top_or_bottom(area, all_areas),
        'mostly_blank': mostly_blank(area, doc_stats),
        'very_separated_words': very_separated_words(area, doc_stats),
        'little_word_coverage': little_word_coverage(area, doc_stats),
        'normal_word_separation': normal_word_separation(area, doc_stats),
        'normal_word_coverage': normal_word_coverage(area, doc_stats),
        'best_caption': best_caption(area),
        'good_caption': good_caption(area),
        'ok_caption': ok_caption(area),
        'overlap': overlap(area, all_areas),
        'proportion_alpha': proportion_alpha(area),
        'offset_words': offset_words(area),
        'n_gaps': len(area['gaps']),
        'n_lines': area['lines'],
        'x1': area['x1'],
        'y1': area['y1'],
        'x2': area['x2'],
        'y2': area['y2'],
        'area': area['area']
    }

def classify_list(area, doc_stats, all_areas):
    return [
        int(has_words(area)),
        int(line_intersect(area, all_areas)),
        int(small_text(area, doc_stats)),
        int(small_leading(area, doc_stats)),
        int(is_line(area)),
        int(is_top_or_bottom(area, all_areas)),
        int(mostly_blank(area, doc_stats)),
        int(very_separated_words(area, doc_stats)),
        int(little_word_coverage(area, doc_stats)),
        int(normal_word_separation(area, doc_stats)),
        int(normal_word_coverage(area, doc_stats)),
        int(best_caption(area)),
        int(good_caption(area)),
        int(ok_caption(area)),
        int(overlap(area, all_areas)),
        int(offset_words(area)),
        proportion_alpha(area),
        area['area'] / float(doc_stats['max_area']),
        len(area['gaps']) / float(doc_stats['max_gaps']),
        area['lines'] / float(doc_stats['max_lines'])
]
