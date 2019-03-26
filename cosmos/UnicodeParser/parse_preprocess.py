from lxml import html, etree
import re
import os, sys
sys.path.append(os.path.dirname(__file__))
import json
import argparse
import loguru
import string

BBOX_COORDINATES_PATTERN = re.compile(".*bbox\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)")
DATA_COORDINATES_PATTERN = re.compile("(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+).")
INPUT_FILE = None

with open('words_alpha.txt') as word_file:
    valid_words = set(word_file.read().split())


def coordinate(title, org_x=0, org_y=0, page_num=0):
    """
    Extract coordinates from ocrx coordinates string
    :param title: ocrx coordinate string
    :param org_x: origin x coordinate
    :param org_y: origin y coordinate
    :param page_num: page number
    :return: dictionary contains ``xmin``, ``ymin``, ``xmax``, ``ymax``, and ``page_number``
    """
    match = BBOX_COORDINATES_PATTERN.search(title)
    # loguru.logger.debug(title)
    return {
        'xmin': int(match.group(1)) + org_x,
        'ymin': int(match.group(2)) + org_y,
        'xmax': int(match.group(3)) + org_x,
        'ymax': int(match.group(4)) + org_y,
        'page_num': int(page_num),
    }


def get_data_coordinate_pattern(data_coordinate_str):
    """
    Extract coordinates from data coordinate string
    :param data_coordinate_str:
    :return: x coordinate, y coordinate
    """
    match = DATA_COORDINATES_PATTERN.search(data_coordinate_str)
    return int(match.group(1)), int(match.group(2))


def load_file_to_tree(path):
    """
    Load file into an etree.
    :param path: Path of file
    :return: Constructed etree
    """
    with open(path, 'r', encoding='utf-8') as in_f:
        doc_str = in_f.read()
        try:
            loaded = etree.fromstring(doc_str)
        except etree.XMLSyntaxError:
            loguru.logger.debug("Invalid XML Change to HTML parser " + path)
            loaded = html.fromstring(doc_str)
            loaded = etree.fromstring(etree.tostring(loaded))
    return loaded


def get_ocr_segments(root):
    """
    Retrieve ocr segment from page tree.
    :param root: Subtree rooted at a single page document tree.
    :return: Generator that outputs ocr segment.
    """
    if 'page' not in root.attrib:
        print('structure broken')
    assert 'page' in root.attrib
    yield from root

def get_all_words_with_coordinates(root):
    """
    Get all words in OCRX and their corresponding coordinates.
    :param root: Tree rooted at a single page.
    :param target: Tag name of the target component.
    :return: Generator outputs coordinate for each word.
    """
    for child in get_ocr_segments(root):
        type = child.attrib['class']
        try:
            #Here the meta_node has been changed to text_unicode
            meta_node = child.xpath(".//*[@class='text_unicode']")[0]
            latex_node = child.xpath(".//*[@class='hocr_img2latex']")[0]
            assert len(child.xpath(".//*[@class='text_unicode']")) == 1
            base_x, base_y = get_data_coordinate_pattern(meta_node.attrib['data-coordinates'])
            page_num = root.attrib['page']
            for word in meta_node.xpath(".//*[@class='ocrx_word']"):
                if word.text and word.text.strip():
                    word_id = word.attrib['id']
                    latex = latex_node.xpath(".//*[@id='"+word_id+"']")[0]
                    yield {
                        'text': word.text,
                        'type': type,
                        'latex': latex.text,
                        'word_bbox': coordinate(word.attrib['title'], base_x, base_y, page_num),
                        'line_bbox': coordinate(word.getparent().attrib['title'], base_x, base_y, page_num),
                        'area_bbox': coordinate(word.getparent().getparent().attrib['title'], base_x, base_y, page_num),
                    }
            #Remove hocr_img2latex and text_unicode tree        
            latex_node.getparent().remove(latex_node)
            meta_node.getparent().remove(meta_node)
        except:
            loguru.logger.debug('hocr class not found ')


def add_name(root):
    """
    Add classification result as a value for name attribute.
    :param root: Subtree rooted at a single section.
    """
    for area in get_ocr_segments(root):
        if 'class' in area.attrib:
            class_name = area.attrib['class']
            for para in area.xpath(".//*[@class='rawtext']/*"):
                para.attrib['name'] = class_name

def all_valid_word(token):
    """
    Check if the words in a token are all in the English dictionary.
    :param token: Input token to be checked.
    :return: True or False.
    """
    token = re.sub('['+string.punctuation+']', '', token)
    words = token.split()
    for word in words:
        if word.lower() not in valid_words:
            return False
    return True

def generate_rawtext_from_ocrx(root):
    """
    Generate content for ``rawtext`` from the ocrx segment.
    :param root: Tree rooted at a single page.
    """
    paths = []
    for ocr_segment in get_ocr_segments(root):
        text_tmp = ''
        if ocr_segment.attrib['class'] == 'Equation':
            try:
                assert len(ocr_segment.xpath(".//*[@class='rawtext']")) == 1
                rawtext_node = ocr_segment.xpath(".//*[@class='rawtext']")[0]
                assert len(ocr_segment.xpath(".//*[@class='equation_unicode']")) == 1
                unicode_node = ocr_segment.xpath(".//*[@class='equation_unicode']")[0]
                #loguru.logger.debug(rawtext_node.text)
                if unicode_node.text:
                    unicode_node.text = unicode_node.text.replace('\n', ' ')
                    unicode_node.text = unicode_node.text.replace('.', ' ')
                    unicode_node.text = unicode_node.text.replace('!', ' ')
                    unicode_node.text = unicode_node.text.replace('?', ' ')
                    unicode_node.text = unicode_node.text.replace(';', ' ')
                    unicode_node.text = re.sub('\(cid:[0-9]*\)', ' ', unicode_node.text)
                    unicode_node.text = re.sub('¼', ' ', unicode_node.text)
                else:
                    unicode_node.text = 'null'
                rawtext_node.text = unicode_node.text
                text_tmp = unicode_node.text
                unicode_node.getparent().remove(unicode_node)
            except AssertionError:
                loguru.logger.debug('Rawtext not found ' )

            if len(text_tmp) > 0 and len(text_tmp.replace('\n','')) > 0:
                for img_elem in ocr_segment.xpath('.//img'):
                    img_path = img_elem.get('src')
                    paths.append(img_path)
            continue
        rawtext = []

        unicode_node = ocr_segment.xpath(".//*[@class='equation_unicode']")[0]
        unicode_node.getparent().remove(unicode_node)
        
        #Here the hocr is actually unicode output
        for hocr in ocr_segment.xpath(".//*[@class='text_unicode']"):
            for paragraph in hocr.xpath(".//*[@class='ocr_par']"):
                words = []
                ocrx_words = paragraph.xpath(".//*[@class='ocrx_word']")
                for word_i in range(len(ocrx_words)-1):
                    if ocrx_words[word_i].text:
                        word_no_space = ocrx_words[word_i].text.strip()
                        if word_no_space.endswith('-') and ocrx_words[word_i+1].text:
                            word_no_space_next = ocrx_words[word_i+1].text.strip()
                            if all_valid_word(word_no_space.rstrip('-')+word_no_space_next):
                                ocrx_words[word_i].text = word_no_space.rstrip('-')+word_no_space_next
                            else:
                                ocrx_words[word_i].text = word_no_space+word_no_space_next
                            ocrx_words[word_i+1].text = ''

                for word in ocrx_words:
            
                    if word.text and word.text.strip():
                        word.text = re.sub('\(cid:[0-9]*\)', '', word.text)
                        word.text = re.sub('¼', '', word.text)

                    if word.text and word.text.strip():
                        words.append(word.text)


                rawtext.append(' '.join(words))
        try:
            assert len(ocr_segment.xpath(".//*[@class='rawtext']")) == 1
            rawtext_node = ocr_segment.xpath(".//*[@class='rawtext']")[0]
            rawtext_node.text = '\n\n'.join(rawtext)
            text_tmp = rawtext_node.text
        except AssertionError:
            loguru.logger.debug('Rawtext not found ' )

        if len(text_tmp) > 0 and len(text_tmp.replace('\n','')) > 0:
            for img_elem in ocr_segment.xpath('.//img'):
                img_path = img_elem.get('src')
                paths.append(img_path)
    
    return paths

def remove_ocr_img_for_non_img(root):
    """
    Remove ``img`` for non-figure class.
    :param root: Tree rooted at a single page.
    """
    for ocr_segment in get_ocr_segments(root):
        is_figure = ocr_segment.attrib['class'] == 'Figure' if 'class' in ocr_segment.attrib else False
        for img_elem in ocr_segment.xpath('.//img'):
            if not is_figure:
                img_elem.getparent().remove(img_elem)


def img_segment_clean_up(root):
    """
    Remove OCR segment for ``Figure`` class.
    :param root: Tree rooted at a single page.
    """
    for child in root.xpath(".//*[@class='Figure']//*"):
        if child.tag == 'img' or ('class' in child.attrib and child.attrib['class'] == 'hocr') or \
            'class' in child.attrib and child.attrib['class'] == 'hocr_img2latex':
            continue
        child.getparent().remove(child)


def remove_ocr_elements(root):
    """
    Remove the OCR component.
    :param root: Tree rooted at a single page.
    """
    for child in root.xpath(".//*[@class='ocr_page']"):
        child.getparent().remove(child)


def split_paragraph(root):
    """
    Split paragraph into ``<p>`` tags delimited by double space.
    :param root: Tree rooted at a single page.
    """
    for area in get_ocr_segments(root):
        for child in area:
            if child.text:
                for paragraph in re.split('\n{2,}', child.text):
                    if len(paragraph) > 0:
                        etree.SubElement(child, "p").text = paragraph
                    child.text = ''

def get_equation(root):
    """
    Get all equations coordinate from subtree of a single page.
    :param root: Tree rooted at a single page.
    :return: Generator that outputs equation coordinates
    """
    for area in get_ocr_segments(root):
        # loguru.logger.debug(area.attrib['id'])
        if area.attrib['class'] == 'Equation':
            page_coord = area.xpath(".//*[@class='hocr']")[0].attrib['data-coordinates']
            base_x, base_y = get_data_coordinate_pattern(
                page_coord
            )
            # loguru.logger.debug(page_coord)
            ocr_coord = area.xpath(".//*[@class='ocr_page']")[0].attrib['title']
            coordinate_info = coordinate(
                title=ocr_coord,
                org_x=base_x,
                org_y=base_y,
                page_num=root.attrib['page'],
            )

            rawtext = area.xpath(".//*[@class='rawtext']")
            if len(rawtext) > 0 and len(rawtext[0]) > 0:
                rawtext = rawtext[0][0]
                coordinate_info['text'] = rawtext.text
            else:
                coordinate_info['text'] = 'null'
            yield coordinate_info




def preprocess(input_file, output_word, output_html, output_equation, output_path, strip_tags):
    """
    Wrapper function that help apply the parser to different pages.
    :param input_file: Location of the folder containing the merged HTML files.
    :param output_word: Location of the folder containing word coordinate json files.
    :param output_html: Intermediate HTML files which will be consumed by the Fonduer parser.
    :param output_equation: Location of the folder containing equation coordinate json files.
    :param output_path: Location of the folder containing source image path json files.
    :param strip_tags: Tags name to be flatten.
    """
    tree = load_file_to_tree(input_file)
    etree.strip_tags(tree, *strip_tags)
    all_words = []
    equations = []
    paths = []
    for page_tree in tree:
        paths += generate_rawtext_from_ocrx(page_tree)
        remove_ocr_img_for_non_img(page_tree)
        #img_segment_clean_up(page_tree)
        split_paragraph(page_tree)
        all_words += [*get_all_words_with_coordinates(page_tree)]
        equations += list(get_equation(page_tree))
        remove_ocr_elements(page_tree)
        add_name(page_tree)

    with open(output_word, 'w') as out_word:
        json.dump(all_words, out_word, indent=4)

    with open(output_html, 'wb') as out_html:
        out_html.write(etree.tostring(tree, pretty_print=True))

    with open(output_equation, 'w') as out_equ:
        json.dump(equations, out_equ, indent=4)

    with open(output_path, 'w') as out_path:
        json.dump(paths, out_path, indent=4)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--input', help="source html", default='data/html/merged/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--output_words', help='location of JSON file that records word and its coordinate', default='out/words/10.4319_lo.1991.36.5.1066.pdf.html.json')
    parser.add_argument('--output_html', help='location of HTML file that has been processed', default='out/html/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--strip_tags', help='Tags to be striped while parsing', nargs='+', default=['strong', 'em'])

    parser.add_argument('--output_equation', default='out/equations/10.4319_lo.1991.36.5.1066.pdf.html')

    args = parser.parse_args()
    preprocess(args.input, args.output_words, args.output_html, args.output_equation, args.strip_tags)

