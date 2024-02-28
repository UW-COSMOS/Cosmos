import pandas as pd
import functools
from PIL import Image
import uuid
import os
import fitz
from .reaggregate_equations import split_equation_system, find_labels_for_equation, save_high_res_img

def check_y_overlap(bb1, bb2):
    _, x1, _, x2 = bb1
    _, y1, _, y2 = bb2
    return y2 >= x1 and x2 >= x1

def aggregate_equations(page_group, write_images_pth, source_pdf):
    targets = []
    objs = []
    for ind, p in page_group.iterrows():
        if p['postprocess_cls'] == 'Equation':
            targets.append(p)
        else:
            objs.append(p)
    final_objs = []
    pymu_pdf = fitz.Document(source_pdf)
    for t in targets:
        pymu_page = pymu_pdf[t['page_num'] - 1]
        sub_regions = split_equation_system(pymu_page, t)
        for region in sub_regions:
            imgid = uuid.uuid4()
            pth = os.path.join(write_images_pth, f'{imgid}.png')
            save_high_res_img(pymu_page, region, pth)
            eq_obj = {'pdf_name': t['pdf_name'],
                    'dataset_id': t['dataset_id'],
                    'detect_score': t['detect_score'],
                    'postprocess_score': t['postprocess_score'],
                    'equation_bb': region,
                    'equation_page': t['page_num'],
                    'content': ','.join(find_labels_for_equation(pymu_page, region)),
                    'img_pth': pth}
            final_objs.append(eq_obj)
    return final_objs


def caption_associate(page_group, caption_class, write_images_pth):
    captions = []
    objs = []
    for ind, p in page_group.iterrows():
        if p['postprocess_cls'] == caption_class:
            captions.append(p)
        else:
            objs.append(p)
    final_objs = []
    for caption in captions:
        cbb = caption['bounding_box']
        tlx, tly, brx, bry = cbb
        mid_x = (tlx + brx) / 2
        mid_y = (tly + bry) / 2
        min_sdist = None
        min_ind = None
        group_obj = {'pdf_name': caption['pdf_name'],
                     'dataset_id': caption['dataset_id'],
                     'caption_content': caption['content'],
                     'caption_page': caption['page_num'],
                     'caption_bb': caption['bounding_box'],
                     'pdf_dims': caption['pdf_dims']}
        if len(objs) == 0:
            continue
        for ind, obj in enumerate(objs):
            tlx, tly, brx, bry = obj['bounding_box']
            o_mid_x = (tlx + brx) / 2
            o_mid_y = (tly + bry) / 2
            sdist = (mid_x - o_mid_x)**2 + (mid_y-o_mid_y)**2
            if min_sdist is None:
                min_sdist = sdist
                min_ind = ind
                continue
            if min_sdist > sdist:
                min_sdist = sdist
                min_ind = ind
        min_obj = objs.pop(min_ind)
        group_obj['content'] = min_obj['content']
        group_obj['obj_page'] = min_obj['page_num']
        group_obj['obj_bbs'] = min_obj['bounding_box']
        group_obj['detect_score'] = min_obj['detect_score']
        group_obj['postprocess_score'] = min_obj['postprocess_score']
        img = Image.open(min_obj['img_pth']).convert('RGB').crop(min_obj['bounding_box'])
        imgid = uuid.uuid4()
        pth = os.path.join(write_images_pth, f'{imgid}.png')
        img.save(pth)
        group_obj['img_pth'] = pth
        final_objs.append(group_obj)
    for obj in objs:
        img = Image.open(obj['img_pth']).convert('RGB').crop(obj['bounding_box'])
        imgid = uuid.uuid4()
        pth = os.path.join(write_images_pth, f'{imgid}.png')
        img.save(pth)
        group_obj = {'pdf_name': obj['pdf_name'],
                     'dataset_id': obj['dataset_id'],
                     'caption_content': None,
                     'caption_page': None,
                     'caption_bb': None,
                     'pdf_dims': obj['pdf_dims'],
                     'detect_score': obj['detect_score'],
                     'postprocess_score': obj['postprocess_score'],
                     'content': obj['content'],
                     'obj_page': obj['page_num'],
                     'obj_bbs': obj['bounding_box'],
                     'img_pth': pth}
        final_objs.append(group_obj)
    return final_objs


def order_page(page_group):
    y_groups = []
    for ind, p in page_group.iterrows():
        grouped_flag = False
        for group in y_groups:
            for member in group:
                overlaps = check_y_overlap(p['bounding_box'], member['bounding_box'])
                if overlaps:
                    group.append(p)
                    grouped_flag = True
                    break
            if grouped_flag:
                break
        if not grouped_flag:
            y_groups.append([p])
    sorted_groups = []
    for group in y_groups:
        slist = sorted(group, key=lambda x: x['bounding_box'][0])
        nested_slist = []
        for obj in slist:
            grouped_flag = False
            for sublist in nested_slist:
                for element in sublist:
                    if abs(element['bounding_box'][0] - obj['bounding_box'][0]) < 20:
                        sublist.append(obj)
                        grouped_flag = True
                        break
                if grouped_flag:
                    break
            if not grouped_flag:
                nested_slist.append([obj])
        internal_sort = []
        for slist in nested_slist:
            internal_sort.append(sorted(slist, key=lambda x:x['bounding_box'][1]))
        sorted_groups.append(internal_sort)
    sorted_groups = sorted(sorted_groups, key=lambda x: x[0][0]['bounding_box'][1])
    final_ordering = []
    for group in sorted_groups:
        for sublist in group:
            for element in sublist:
                final_ordering.append(element)
    return final_ordering


def group_section(obj_list):
    section = {'pdf_name': obj_list[0]['pdf_name'], 'dataset_id': obj_list[0]['dataset_id']}
    section['detect_score'] = obj_list[0]['detect_score']
    section['postprocess_score'] = obj_list[0]['postprocess_score']
    if obj_list[0]['postprocess_cls'] == 'Section Header':
        section['section_header'] = obj_list[0]['content']
        section['section_header_page'] = obj_list[0]['page_num']
        section['section_header_bb'] = obj_list[0]['bounding_box']
        obj_list.pop(0)
    content = [obj['content'] for obj in obj_list]
    content = ' '.join(content)
    section['content'] = content
    section['obj_pages'] = [obj['page_num'] for obj in obj_list]
    section['obj_bbs'] = [obj['bounding_box'] for obj in obj_list]

    return section


def aggregate_sections(pdf):
    pdf = pdf[pdf['postprocess_cls'].isin(['Body Text', 'Section Header'])]
    grouped = pdf.groupby('page_num').apply(order_page)
    final_ordering = []
    for ind, order in grouped.items():
        final_ordering.extend(order)
    sections = [[]]
    for item in final_ordering:
        if item['postprocess_cls'] == 'Section Header':
            sections.append([item])
        else:
            sections[-1].append(item)
    sections = [group_section(s) for s in sections if len(s) > 0]
    return sections


def aggregate_tables(pdf, write_images_pth):
    pdf = pdf[pdf['postprocess_cls'].isin(['Table', 'Table Caption'])]
    tc_associate = functools.partial(caption_associate, caption_class='Table Caption', write_images_pth=write_images_pth)
    grouped = pdf.groupby('page_num').apply(tc_associate)
    final_ordering = []
    for ind, order in grouped.items():
        final_ordering.extend(order)
    return final_ordering


def aggregate_figures(pdf, write_images_pth):
    pdf = pdf[pdf['postprocess_cls'].isin(['Figure', 'Figure Caption'])]
    tc_associate = functools.partial(caption_associate, caption_class='Figure Caption', write_images_pth=write_images_pth)
    grouped = pdf.groupby('page_num').apply(tc_associate)
    final_ordering = []
    for ind, order in grouped.items():
        final_ordering.extend(order)
    return final_ordering


def aggregate_pdf(pdf):
    pdf_obj = {}
    content = ''
    obj_pages = []
    obj_bbs = []
    for ind, row in pdf.iterrows():
        if 'pdf_name' not in pdf_obj:
            pdf_obj['pdf_name'] = row['pdf_name']
        if 'dataset_id' not in pdf_obj:
            pdf_obj['dataset_id'] = row['dataset_id']
        content += f' {row["content"]}'
        obj_pages.append(row['page_num'])
        obj_bbs.append(row['bounding_box'])
    pdf_obj['content'] = content
    pdf_obj['obj_pages'] = obj_pages
    pdf_obj['obj_bbs'] = obj_bbs
    return pdf_obj


stream_types = ['sections', 'pdfs']
association_types = ['tables', 'figures']
full_page_types = ['equations']


def aggregate_router(ddf, aggregate_type, write_images_pth, source_pdf=None):
    if aggregate_type in stream_types:
        return stream_aggregate(ddf, aggregate_type)
    elif aggregate_type in association_types:
        return association_aggregate(ddf, aggregate_type, write_images_pth)
    elif aggregate_type in full_page_types:
        return full_page_aggregate(ddf, aggregate_type, write_images_pth, source_pdf)
    else:
        raise ValueError(f'Passed type not support for aggregation. Supported types are {stream_types + association_types}')


def full_page_aggregate(ddf, aggregate_type, write_images_pth,source_pdf=None):
    if aggregate_type == 'equations':
        ae = functools.partial(aggregate_equations, write_images_pth=write_images_pth, source_pdf=source_pdf)
        result = ddf.groupby('pdf_name').apply(ae)
        results = []
        for pdf_name, sections in result.items():
            for section in sections:
                results.append(section)
        results_df = pd.DataFrame(results)
        return results_df


def stream_aggregate(ddf, aggregate_type):
    if aggregate_type == 'sections':
        result = ddf.groupby('pdf_name').apply(aggregate_sections)
        results = []
        for pdf_name, sections in result.items():
            for section in sections:
                results.append(section)
        results_df = pd.DataFrame(results)
        return results_df
    if aggregate_type == 'pdfs':
        result = ddf.groupby('pdf_name').apply(aggregate_pdf)
        results = []
        for pdf_name, item in result.items():
            results.append(item)
        result_df = pd.DataFrame(results)
        return result_df


def association_aggregate(ddf, aggregate_type, write_images_pth):
    if aggregate_type == 'tables':
        atab = functools.partial(aggregate_tables, write_images_pth=write_images_pth)
        result = ddf.groupby('pdf_name').apply(atab)
        results = []
        for pdf_name, tables in result.items():
            for table in tables:
                results.append(table)
        results_df = pd.DataFrame(results)
        return results_df
    if aggregate_type == 'figures':
        afig = functools.partial(aggregate_figures, write_images_pth=write_images_pth)
        result = ddf.groupby('pdf_name').apply(afig)
        results = []
        for pdf_name, tables in result.items():
            for table in tables:
                results.append(table)
        results_df = pd.DataFrame(results)
        return results_df
