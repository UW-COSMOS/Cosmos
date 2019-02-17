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


def coordinate(title,org_x=0, org_y=0, page_num=0,):
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
    match = DATA_COORDINATES_PATTERN.search(data_coordinate_str)
    return int(match.group(1)), int(match.group(2))


def load_file_to_tree(path):
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
    if 'page' not in root.attrib:
        print(INPUT_FILE)
    assert 'page' in root.attrib
    yield from root


def get_all_words_with_coordinates(root):
    for child in get_ocr_segments(root):
        type = child.attrib['class']
        try:
            meta_node = child.xpath(".//*[@class='hocr']")[0]
            assert len(child.xpath(".//*[@class='hocr']")) == 1
            base_x, base_y = get_data_coordinate_pattern(meta_node.attrib['data-coordinates'])
            page_num = root.attrib['page']
            for word in child.xpath(".//*[@class='ocrx_word']"):
                if word.text.strip():
                    # print(word.text)
                    yield {
                        'text': word.text,
                        'type': type,
                        'word_bbox': coordinate(word.attrib['title'], base_x, base_y, page_num),
                        'line_bbox': coordinate(word.getparent().attrib['title'], base_x, base_y, page_num),
                        'area_bbox': coordinate(word.getparent().getparent().attrib['title'], base_x, base_y, page_num),
                    }
        except:
            loguru.logger.debug('hocr class not found ' + INPUT_FILE)


def add_name(root):
    for area in get_ocr_segments(root):
        if 'class' in area.attrib:
            class_name = area.attrib['class']
            for para in area.xpath(".//*[@class='rawtext']/*"):
                para.attrib['name'] = class_name


def generate_rawtext_from_ocrx(root):
    for ocr_segment in get_ocr_segments(root):
        if ocr_segment.attrib['class'] == 'Equation':
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
    for ocr_segment in get_ocr_segments(root):
        is_figure = ocr_segment.attrib['class'] == 'Figure' if 'class' in ocr_segment.attrib else False
        for img_elem in ocr_segment.xpath('.//img'):
            if not is_figure:
                img_elem.getparent().remove(img_elem)


def img_segment_clean_up(root):
    for child in root.xpath(".//*[@class='Figure']//*"):
        if child.tag == 'img' or ('class' in child.attrib and child.attrib['class'] == 'hocr'):
            continue
        child.getparent().remove(child)


def remove_ocr_elements(root):
    for child in root.xpath(".//*[@class='ocr_page']"):
        child.getparent().remove(child)


def split_paragraph(root):
    for area in get_ocr_segments(root):
        for child in area:
            if child.text:
                for paragraph in re.split('\n{2,}', child.text):
                    if len(paragraph) > 0:
                        etree.SubElement(child, "p").text = paragraph
                    child.text = ''

def get_equation(root):
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




def preprocess(input_file, output_word, output_html, output_equation, strip_tags):
    tree = load_file_to_tree(input_file)
    etree.strip_tags(tree, *strip_tags)
    all_words = []
    equations = []
    # print(tree.attrib)
    for page_tree in tree:
        generate_rawtext_from_ocrx(page_tree)
        remove_ocr_img_for_non_img(page_tree)
        img_segment_clean_up(page_tree)
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


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--input', help="source html", default='data/html/merged/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--output_words', help='location of JSON file that records word and its coordinate', default='out/words/10.4319_lo.1991.36.5.1066.pdf.html.json')
    parser.add_argument('--output_html', help='location of HTML file that has been processed', default='out/html/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--strip_tags', help='Tags to be striped while parsing', nargs='+', default=['strong', 'em'])

    parser.add_argument('--output_equation', default='out/equations/10.4319_lo.1991.36.5.1066.pdf.html')

    args = parser.parse_args()
    preprocess(args.input, args.output_words, args.output_html, args.output_equation, args.strip_tags)
