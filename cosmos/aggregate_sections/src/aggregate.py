"""
Aggregate text blobs into coherent sections
"""

import logging
import time
import os
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
from collections import defaultdict
from joblib import Parallel, delayed
import click

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, defer
from sqlalchemy.sql.expression import func
from schema import Pdf, Page, PageObject, Section

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
Session = sessionmaker()
Session.configure(bind=engine)

MIN_SECTION_LEN = 30

def load_pages(buffer_size):
    current_docs = []
    session = Session()
    res = session.execute('SELECT id FROM pdfs')
    for pdf in res:
        pdf_id = pdf.id

        # Find all objects
        res = session.query(Page, PageObject).filter(Page.pdf_id == pdf_id)\
                .filter(Page.id == PageObject.page_id)

        obj_list = []
        for page, po in res:
            obj_list.append({'page' : page, 'page_object': po})

        current_docs.append(obj_list)
        if len(current_docs) == buffer_size:
            yield current_docs
            current_docs = []
    yield current_docs


def groups(pobjs):
    groups = []
    for p in pobjs:
        bb = p['page_object'].bounding_box
        x1, y1, x2, y2 = bb
        inserted = False
        for group in groups:
            gx1 = group[0]
            if gx1-50 <= x1 <= gx1+50:
                group[1].append(p)
                inserted = True
                break
        if not inserted:
            groups.append([x1, [p]])
    for g in groups:
        # sort internally by y
        g[1].sort(key=lambda x: x['page_object'].bounding_box[1])
    # Sort bins by x
    groups.sort(key=lambda x: x[0])
    return groups

def aggregate_equations(objs):
    def filter_fn(obj):
        return obj.page_object['class'] in ['Equation', 'Body Text']

    fobjs = [o for o in objs if filter_fn(o)]
    pages = defaultdict(list)
    for obj in fobjs:
        pages[obj.page['page_num']].append(obj)

    keys = pages.keys()
    if len(keys) == 0:
        return []

    max_len = max(keys)
    equations = []
    for i in range(max_len):
        if i not in pages:
            continue
        page_objs = pages[i]
        grouped = groups(page_objs)
        for gind, group in enumerate(grouped):
            for ind, obj in enumerate(group[1]):
                if obj['class'] == 'Equation':
                    # Try to grab nearest body texts
                    assoc_text = []
                    if ind-1 >= 0 and group[1][ind-1]['class'] == 'Body Text':
                        assoc_text.append(group[1][ind-1])
                    if ind+1 < len(group[1]):
                        if group[1][ind+1]['class'] == 'Body Text':
                            assoc_text.append(group[1][ind+1])
                    elif gind+1 < len(grouped) and grouped[gind+1][1][0]['class'] == 'Body Text': # Check the first item in the next group
                        assoc_text.append(grouped[gind+1][1][0])

                    equations.append([obj, assoc_text])

    final_objs = []
    for eq_contexts in equations:
        eq, contexts = eq_contexts
        pdf_name = eq['pdf_name']
        aggregated_context = ''
        for context in contexts:
            aggregated_context += f'\n{context["content"]}\n'
        if aggregated_context.strip() == '' or len(aggregated_context.strip()) < MIN_SECTION_LEN:
            continue
        contexts = [{'_id': context['_id']} for context in contexts]
        final_obj = {'equation': eq,
                     'contexts': contexts,
                     'content': aggregated_context,
                     'class': 'EquationContext',
                     'pdf_name': pdf_name}
        final_objs.append(final_obj)
    import pdb; pdb.set_trace()
    return final_objs


def aggregate_figures(objs):
    def filter_fn(obj):
        return obj['class'] in ['Figure', 'Figure Caption']

    fobjs = [o for o in objs if filter_fn(o)]
    pages = defaultdict(list)
    for obj in fobjs:
        pages[obj['page_num']].append(obj)

    keys = pages.keys()
    if len(keys) == 0:
        return []

    max_len = max(keys)
    figures = []
    after = False
    for i in range(max_len):
        if i not in pages:
            continue
        page_objs = pages[i]
        grouped = groups(page_objs)
        flattened = [o for g in grouped for o in g[1]]
        if flattened[0]['class'] == 'Figure':
            after = True
        break

    for i in range(max_len):
        if i not in pages:
            continue
        page_objs = pages[i]
        grouped = groups(page_objs)
        flattened = [o for g in grouped for o in g[1]]
        for ind, obj in enumerate(flattened):
            if obj['class'] == 'Figure Caption':
                if after and (ind-1) > 0 and flattened[ind-1]['class'] == 'Figure':
                    figures.append([flattened[ind-1], flattened[ind]])
                if not after and (ind+1) < len(flattened) and flattened[ind+1]['class'] == 'Figure':
                    figures.append([flattened[ind+1], flattened[ind]])

    final_objs = []
    for fig_fig_caption in figures:
        fig, fig_caption = fig_fig_caption
        pdf_name = fig['pdf_name']
        aggregated_context = fig_caption['content']
        if aggregated_context.strip() == '':
            continue
        fig_caption = {'_id': fig_caption['_id']}
        final_obj = {'figure': fig,
                     'caption': fig_caption,
                     'content': aggregated_context,
                     'class': 'FigureContext',
                     'pdf_name': pdf_name}
        final_objs.append(final_obj)
    return final_objs



def aggregate_tables(objs):
    def filter_fn(obj):
        return obj['class'] in ['Table', 'Table Caption']

    fobjs = [o for o in objs if filter_fn(o)]
    pages = defaultdict(list)
    for obj in fobjs:
        pages[obj['page_num']].append(obj)

    keys = pages.keys()
    if len(keys) == 0:
        return []

    max_len = max(keys)
    tables = []
    after = False
    for i in range(max_len):
        if i not in pages:
            continue
        page_objs = pages[i]
        grouped = groups(page_objs)
        flattened = [o for g in grouped for o in g[1]]
        if flattened[0]['class'] == 'Table':
            after = True
        break

    for i in range(max_len):
        if i not in pages:
            continue
        page_objs = pages[i]
        grouped = groups(page_objs)
        flattened = [o for g in grouped for o in g[1]]
        for ind, obj in enumerate(flattened):
            if obj['class'] == 'Table':
                if not after:
                    if (ind-1) > 0 and flattened[ind-1]['class'] == 'Table Caption':
                        tables.append([flattened[ind], flattened[ind-1]])
                    else:
                        tables.append([flattened[ind], None])

                if after:
                    if (ind+1) < len(flattened) and flattened[ind+1]['class'] == 'Table Caption':
                        tables.append([flattened[ind], flattened[ind+1]])
                    else:
                        tables.append([flattened[ind], None])

    final_objs = []
    for tab_tab_caption in tables:
        tab, tab_caption = tab_tab_caption
        pdf_name = tab['pdf_name']
        aggregated_context = f"{tab['content']}\n{tab_caption['content']}" if tab_caption is not None else tab['content']
        if aggregated_context.strip() == '':
            continue
        tab_caption = {'_id': tab_caption['_id']} if tab_caption is not None else None
        final_obj = {'table': tab,
                     'caption': tab_caption,
                     'content': aggregated_context,
                     'class': 'TableContext',
                     'pdf_name': pdf_name}
        final_objs.append(final_obj)
    return final_objs


def aggregate_sections(objs):
    def filter_fn(obj):
        print(obj['page_object'].cls)
        return obj['page_object'].cls in ['Body Text', 'Section Header']

    fobjs = [o for o in objs if filter_fn(o)]
    pages = defaultdict(list)
    for obj in fobjs:
        pages[obj['page'].page_number].append(obj)
    keys = pages.keys()
    if len(keys) == 0:
        # Probably should log something here
        return []
    max_len = max(keys)
    sections = []
    for i in range(max_len + 1):
        if i not in pages:
            print(":(")
            continue
        page_objs = pages[i]
        grouped = groups(page_objs)
        current_section = None
        for group in grouped:
            for obj in group[1]:
                if obj['page_object'].cls == 'Section Header':
                    current_section = obj
                    sections.append([obj, []])
                    continue
                if current_section is None:
                    if len(sections) == 0:
                        sections.append([None, [obj]])
                        continue
                sections[-1][1].append(obj)

    final_objs = []
    for section in sections:
        header, objs = section
        aggregated_context = ''
        pdf_id = ''
        if header is not None:
            aggregated_context = f'{header["page_object"].content}\n'
            pdf_id = header['page'].pdf_id
        else:
            pdf_id = objs[0]['page'].pdf_id
        for obj in objs:
            aggregated_context += f'\n{obj["page_object"].content}\n'
        if aggregated_context.strip() == '' or len(aggregated_context.strip()) < MIN_SECTION_LEN:
            continue
        objs = [{'_id': obj['page_object'].id} for obj in objs]
        # TODO: make this a model
        final_obj = {'header': header,
                     'objects': objs,
                     'content': aggregated_context,
                     'class': 'Section',
                     'pdf_id': pdf_id}
        final_objs.append(final_obj)
    return final_objs

def section_scan(db_section_insert_fn, db_equation_insert_fn, db_figure_insert_fn, db_table_insert_fn, num_processes, sections, equations, figures, tables):
    logging.info('Starting section extraction over objects')
    start_time = time.time()
    for batch in load_pages(num_processes):
        logging.info('Batch constructed. Running aggregation')
        if sections:
            objs = Parallel(n_jobs=num_processes)(delayed(aggregate_sections)(o) for o in batch)
            objs = [o for p in objs for o in p]
            if len(objs) == 0:
                continue
            db_section_insert_fn(objs)

        # TODO: Maybe bring these back?
#        if equations:
#            objs = Parallel(n_jobs=num_processes)(delayed(aggregate_equations)(o) for o in batch)
#            objs = [o for p in objs for o in p]
#            if len(objs) == 0:
#                continue
#            db_equation_insert_fn(objs, client)
#
#        if figures:
#            objs = Parallel(n_jobs=num_processes)(delayed(aggregate_figures)(o) for o in batch)
#            objs = [o for p in objs for o in p]
#            if len(objs) == 0:
#                continue
#            db_figure_insert_fn(objs, client)
#
#        if tables:
#            objs = Parallel(n_jobs=num_processes)(delayed(aggregate_tables)(o) for o in batch)
#            objs = [o for p in objs for o in p]
#            if len(objs) == 0:
#                continue
#            db_table_insert_fn(objs, client)

def section_insert_fn(objs):
    session = Session()
    for obj in objs:
#    page_id = Column(Integer, ForeignKey('pages.id'))
#    header = Column(String(200))
#    content = Column(String(10000))
#    objects = Column(ARRAY(Integer))
#    cls = Column(String(200))
        section = Section(pdf_id=obj['pdf_id'],
                header=obj['header'],
                content=obj['content'],
                objects=','.join([str(i['_id']) for i in obj['objects']]),
                cls=obj['class'],
                )
        session.add(section)
        session.commit()

        print(obj['content'])
        print('-----------------------------------------')


#    result = db.sections.insert_many(objs)
    logging.info(f'Inserted result: {result}')


def equation_insert_fn(objs, client):
    db = client.pdfs
    result = db.equationContexts.insert_many(objs)
    logging.info(f'Inserted result: {result}')


def figure_insert_fn(objs, client):
    db = client.pdfs
    result = db.figureContexts.insert_many(objs)
    logging.info(f'Inserted result: {result}')


def table_insert_fn(objs, client):
    db = client.pdfs
    result = db.tableContexts.insert_many(objs)
    logging.info(f'Inserted result: {result}')


@click.command()
@click.argument('num_processes')
@click.option('--section/--no-section')
@click.option('--equation/--no-equation')
@click.option('--figure/--no-figure')
@click.option('--table/--no-table')
def run(num_processes, section, equation, figure, table):
    section_scan(section_insert_fn, equation_insert_fn, figure_insert_fn, table_insert_fn, int(num_processes), section, equation, figure, table)


if __name__ == '__main__':
    run()


