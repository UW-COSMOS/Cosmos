

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
    section = {'pdf_name': obj_list[0]['pdf_name']}
    if obj_list[0]['postprocess_cls'] == 'Section Header':
        section['section_header'] = obj_list[0]['content']
        section['section_header_key'] = (obj_list[0]['page_num'], obj_list[0]['bounding_box'])
        obj_list.pop(0)
    content = [obj['content'] for obj in obj_list]
    content = ' '.join(content)
    section['content'] = content
    section['obj_keys'] = [(obj['page_num'], obj['bounding_box']) for obj in obj_list]
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
    pdf = {}
    content = ''
    obj_keys = []
    for ind, row in pdf.iterrows():
        if 'pdf_name' not in pdf:
            pdf['pdf_name'] = row['pdf_name']
        content += f' {row["content"]}'
        obj_keys.append((row['page_num'], row['bounding_box']))
    pdf['content'] = content
    pdf['obj_keys'] = obj_keys
    return pdf


supported_types = ['sections', 'pdfs']


def stream_aggregate(ddf, aggregate_type='sections'):
    if aggregate_type not in supported_types:
        raise ValueError(f'Passed type not supported for stream aggregation. Acceptable types are: {supported_types}')
    if aggregate_type == 'sections':
        return ddf.groupby('pdf_name').apply(aggregate_sections, meta=object).compute()
    if aggregate_type == 'pdfs':
        return ddf.groupby('pdf_name').apply(aggregate_pdf, meta=object).compute()

