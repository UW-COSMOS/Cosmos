import pandas as pd

def check_y_overlap(bb1, bb2):
    _, x1, _, x2 = bb1
    _, y1, _, y2 = bb2
    return y2 >= x1 and x2 >= x1


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
    for ind, order in grouped.iteritems():
        final_ordering.extend(order)
    sections = [[]]
    for item in final_ordering:
        if item['postprocess_cls'] == 'Section Header':
            sections.append([item])
        else:
            sections[-1].append(item)
    sections = [group_section(s) for s in sections if len(s) > 0]
    return sections


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
association_types = []


def aggregate_router(ddf, aggregate_type):
    if aggregate_type in stream_types:
        return stream_aggregate(ddf, aggregate_type)
    elif aggregate_type in association_types:
        association_aggregate(ddf, aggregate_type)
    else:
        raise ValueError(f'Passed type not support for aggregation. Supported types are {stream_types + association_types}')


def stream_aggregate(ddf, aggregate_type):
    if aggregate_type == 'sections':
        result = ddf.groupby('pdf_name').apply(aggregate_sections)
        results = []
        for pdf_name, sections in result.iteritems():
            for section in sections:
                results.append(section)
        results_df = pd.DataFrame(results)
        return results_df
    if aggregate_type == 'pdfs':
        result = ddf.groupby('pdf_name').apply(aggregate_pdf)
        results = []
        for pdf_name, item in result.iteritems():
            results.append(item)
        result_df = pd.DataFrame(results)
        return result_df


def association_aggregate(ddf, aggregate_type):
    raise NotImplementedError('Implement association aggregate')