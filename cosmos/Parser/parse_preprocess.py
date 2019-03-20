from lxml import html, etree
import re
import os, sys

sys.path.append(os.path.dirname(__file__))
import json
import argparse
import loguru

BBOX_COORDINATES_PATTERN = re.compile(".*bbox\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)")
DATA_COORDINATES_PATTERN = re.compile("(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+).")
INPUT_FILE = None


def coordinate(title, org_x=0, org_y=0, page_num=0, ):
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
        'page_num': int(page_num) if page_num is not None else None,
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
    Retrieve ocr segment from page tree
    :param root: Subtree rooted at a single page document tree
    :return: Generator that outputs ocr segment.
    """
    if 'page' not in root.attrib:
        print(INPUT_FILE)
    yield from root


def get_words_from_child(root, target, type=None):
    """
    Get all words in OCRX and their corresponding coordinates.
    :param root: Subtree rooted at component for different segmentation
    :param target: Tag name of the target component.
    :param type: Type from classifier
    :return: Generator outputs coordinate for each word
    """
    meta_nodes = root.xpath(f".//*[@class='{target}']")
    if len(meta_nodes) == 0:
        print(etree.tostring(root, pretty_print=True))
        print('----')
        print(target)
        raise Exception('Unable to find target on root')
    meta_node = meta_nodes[0]
    base_x, base_y = get_data_coordinate_pattern(meta_node.attrib['data-coordinates'])
    page_num = None
    if 'page' in root.attrib:
        page_num = root.attrib['page']
    for word in root.xpath(".//*[@class='ocrx_word']"):
        if word.text is None:
            continue
        if word.text.strip():
            # print(word.text)
            yield {
                'text': word.text,
                'type': type,
                'word_bbox': coordinate(word.attrib['title'], base_x, base_y, page_num),
                'line_bbox': coordinate(word.getparent().attrib['title'], base_x, base_y, page_num),
                'area_bbox': coordinate(word.getparent().getparent().attrib['title'], base_x, base_y, page_num),
            }


def get_all_words_with_coordinates(root, target='hocr'):
    """
    Get all words in OCRX and their corresponding coordinates.
    :param root: Tree rooted at a single page.
    :param target: Tag name of the target component.
    :return: Generator outputs coordinate for each word
    """
    for child in get_ocr_segments(root):
        type = child.attrib['class']
        words = get_words_from_child(child, target, type=type)
        yield from words


def add_name(root):
    '''
    Add classification result as a value for name attribute.
    :param root:
    :return:
    '''
    for area in get_ocr_segments(root):
        if 'class' in area.attrib:
            class_name = area.attrib['class']
            for para in area.xpath(".//*[@class='rawtext']/*"):
                para.attrib['name'] = class_name


def generate_rawtext_from_ocrx(root):
    '''
    Generate content for ``rawtext`` from the ocrx segment.
    :param root: Tree rooted at a single page.
    '''
    for ocr_segment in get_ocr_segments(root):
        if ocr_segment.attrib['class'] == 'Equation':
            try:
                rawtext_node = ocr_segment.xpath(".//*[@class='rawtext']")[0]
                rawtext_node.text = rawtext_node.text.replace('\n', ' ')
                rawtext_node.text = rawtext_node.text.replace('.', ' ')
            except:
                pass
            continue
        rawtext = []
        for paragraph in ocr_segment.xpath(".//*[@class='ocr_par']"):
            words = []
            for word in paragraph.xpath(".//*[@class='ocrx_word']"):
                words.append(word.text)
            rawtext.append(' '.join(words))
        try:
            assert len(ocr_segment.xpath(".//*[@class='rawtext']")) == 1
            rawtext_node = ocr_segment.xpath(".//*[@class='rawtext']")[0]
            rawtext_node.text = '\n\n'.join(rawtext)
        except AssertionError:
            loguru.logger.debug('Rawtext not found ' + INPUT_FILE)


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
        if child.tag == 'img' or ('class' in child.attrib and child.attrib['class'] == 'hocr'):
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
            yield coordinate(
                title=ocr_coord,
                org_x=base_x,
                org_y=base_y,
                page_num=root.attrib['page']
            )


def preprocess(tree, filename, strip_tags):
    """
    Wrapper function that help apply the parser to different pages.
    :param tree: Document tree that contains different page tree as children
    :param filename: Filename of the output HTML file
    :param strip_tags: Tags name to be flatten
    :return: All words and equations from this file (with coordinate information)
    """
    etree.strip_tags(tree, *strip_tags)
    words = []
    equations = []
    for page_tree in tree:
        generate_rawtext_from_ocrx(page_tree)
        remove_ocr_img_for_non_img(page_tree)
        img_segment_clean_up(page_tree)
        split_paragraph(page_tree)
        words += [*get_all_words_with_coordinates(page_tree)]
        equations += list(get_equation(page_tree))
        remove_ocr_elements(page_tree)
        add_name(page_tree)

    with open(filename, 'wb') as out_html:
        out_html.write(etree.tostring(tree, pretty_print=True))

    return words, equations


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--input', help="source html", default='data/html/merged/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--output_words', help='location of JSON file that records word and its coordinate',
                        default='out/words/10.4319_lo.1991.36.5.1066.pdf.html.json')
    parser.add_argument('--output_html', help='location of HTML file that has been processed',
                        default='out/html/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--strip_tags', help='Tags to be striped while parsing', nargs='+', default=['strong', 'em'])

    parser.add_argument('--output_equation', default='out/equations/10.4319_lo.1991.36.5.1066.pdf.html')

    args = parser.parse_args()
    preprocess(args.input, args.output_words, args.output_html, args.output_equation, args.strip_tags)
