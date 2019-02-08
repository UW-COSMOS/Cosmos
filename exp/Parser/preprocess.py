from lxml import html, etree
import re
import json
import argparse
import loguru

BBOX_COORDINATES_PATTERN = re.compile("bbox\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)")
DATA_COORDINATES_PATTERN = re.compile("(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+)\s(-?[0-9]+).")
INPUT_FILE = None

def coordinate(title, org_x=0, org_y=0, page_num=0):
    match = BBOX_COORDINATES_PATTERN.search(title)
    return {
        'xmin': int(match.group(1)) + org_x,
        'ymin': int(match.group(2)) + org_y,
        'xmax': int(match.group(3)) + org_x,
        'ymax': int(match.group(4)) + org_y,
        'page_num': int(page_num)
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
            loguru.logger.debug("Invalid XML Change to HTML parser "+path)
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
                        'word_bbox': coordinate(word.attrib['title'], base_x, base_y, page_num),
                        'line_bbox': coordinate(word.getparent().attrib['title'], base_x, base_y, page_num),
                        'area_bbox': coordinate(word.getparent().getparent().attrib['title'], base_x, base_y, page_num),
                    }
        except:
            loguru.logger.debug('hocr class not found '+INPUT_FILE)




def add_name(root):
    for area in get_ocr_segments(root):
        if 'class' in area.attrib:
            class_name = area.attrib['class']
            for para in area.xpath(".//*[@class='rawtext']/*"):
                para.attrib['name'] = class_name


def generate_rawtext_from_ocrx(root):
    for ocr_segment in get_ocr_segments(root):
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
            loguru.logger.debug('Rawtext not found '+INPUT_FILE)




def remove_ocr_img_for_non_img(root):
    for ocr_segment in get_ocr_segments(root):
        is_figure = ocr_segment.attrib['class'] == 'Figure' if 'class' in ocr_segment.attrib else False
        for img_elem in ocr_segment.xpath('.//img'):
            if not is_figure:
                img_elem.getparent().remove(img_elem)


def img_segment_clean_up(root):
    for child in root.xpath(".//*[@class='Figure']//*"):
        if child.tag == 'img' or ('class' in child.attrib and child.attrib['class']=='hocr'):
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


def preprocess(input_file, output_word, output_html, strip_tags):
    tree = load_file_to_tree(input_file)
    etree.strip_tags(tree, *strip_tags)
    all_words = []
    # print(tree.attrib)
    for page_tree in tree:
        generate_rawtext_from_ocrx(page_tree)
        remove_ocr_img_for_non_img(page_tree)
        img_segment_clean_up(page_tree)
        split_paragraph(page_tree)
        all_words += [*get_all_words_with_coordinates(page_tree)]
        remove_ocr_elements(page_tree)
        add_name(page_tree)

    with open(output_word, 'w') as out_word:
        json.dump(all_words, out_word, indent=4)

    with open(output_html, 'wb') as out_html:
        out_html.write(etree.tostring(tree, pretty_print=True))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', help="source html", default='data/html/merged/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--output_words', help='location of JSON file that records word and its coordinate', default='out/words/10.4319_lo.1991.36.5.1066.pdf.html.json')
    parser.add_argument('--output_html', help='location of HTML file that has been processed', default='out/html/10.4319_lo.1991.36.5.1066.pdf.html')
    parser.add_argument('--strip_tags', help='Tags to be striped while parsing', nargs='+', default=['strong', 'em'])
    args = parser.parse_args()
    preprocess(args.input, args.outpu_word, args.output_html, args.strip_tags)
