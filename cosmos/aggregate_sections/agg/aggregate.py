"""
Aggregate text blobs into coherent sections
"""

import time
import os
from collections import defaultdict
from joblib import Parallel, delayed
import click

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, defer
from sqlalchemy.sql.expression import func
from sqlalchemy.sql import text
from agg.schema import Pdf, Page, PageObject, Section, ObjectContext
from dask.distributed import Client, progress


import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
MIN_SECTION_LEN = 30


def process_dataset(did, client):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    conn = engine.connect()
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    final_context_futures = []
    try:
        q = text('SELECT id from pdfs where pdfs.dataset_id = :did')
        res = conn.execute(q, did=did)
        object_context_futures = []
        for pdf in res:
            pdf_id = pdf.id
            res2 = session.query(Page, PageObject).filter(Page.pdf_id == pdf_id)\
                    .filter(Page.id == PageObject.page_id)

            obj_list = []
            for page, po in res2:
                obj = {'page' : page, 'page_object': po}
                obj_list.append(obj)
                
            object_context_futures.append(client.submit(aggregate_sections, obj_list, resources={'process': 1}))
            object_context_futures.append(client.submit(aggregate_equations, obj_list, resources={'process': 1}))
            object_context_futures.append(client.submit(aggregate_figures, obj_list, resources={'process': 1}))
            object_context_futures.append(client.submit(aggregate_tables, obj_list, resources={'process': 1}))
            final_context_futures.extend(object_context_futures)
        progress(final_context_futures)
        client.gather(final_context_futures)
    except Exception as e:
        logger.error(str(e), exc_info=True)
        raise Exception(f'process_dataset error, {str(e)}')
    finally:
        session.close()


def groups(pobjs):
    """
    groups
    Group objects on the page by location
    :param pobjs: Input list of objects [{'page_object': PageObject, 'page': Page}]
    :return: list of groups sorted by x coordinate [[topleft_x_coord, [pobjs]]]
    """
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
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    try:
        def filter_fn(obj):
            return obj['page_object'].cls in ['Equation', 'Body Text']

        fobjs = [o for o in objs if filter_fn(o)]
        pages = defaultdict(list)
        for obj in fobjs:
            pages[obj['page'].page_number].append(obj)

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
                    if obj['page_object'].cls == 'Equation':
                        # Try to grab nearest body texts
                        assoc_text = []
                        if ind-1 >= 0 and group[1][ind-1]['page_object'].cls == 'Body Text':
                            assoc_text.append(group[1][ind-1])
                        if ind+1 < len(group[1]):
                            if group[1][ind+1]['page_object'].cls == 'Body Text':
                                assoc_text.append(group[1][ind+1])
                        elif gind+1 < len(grouped) and grouped[gind+1][1][0]['page_object'].cls == 'Body Text': # Check the first item in the next group
                            assoc_text.append(grouped[gind+1][1][0])

                        equations.append([obj, assoc_text])

        final_objs = []
        for eq_contexts in equations:
            eq, contexts = eq_contexts
            aggregated_context = ''
            for context in contexts:
                aggregated_context += f'\n{context["page_object"].content}\n'
            if aggregated_context.strip() == '' or len(aggregated_context.strip()) < MIN_SECTION_LEN:
                continue
            oc = ObjectContext(pdf_id=eq['page'].pdf_id,
                               cls='EquationContext',
                               header_id=eq["page_object"].id,
                               header_content=eq["page_object"].content,
                               content=aggregated_context)
            session.add(oc)
        session.commit()
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()
    return 'Ok'

def aggregate_figures(objs):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    def filter_fn(obj):
        return obj['page_object'].cls in ['Figure', 'Figure Caption']
    try:
        fobjs = [o for o in objs if filter_fn(o)]
        pages = defaultdict(list)
        for obj in fobjs:
            pages[obj['page'].page_number].append(obj)

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
            if flattened[0]['page_object'].cls == 'Figure':
                after = True
            break

        for i in range(max_len):
            if i not in pages:
                continue
            page_objs = pages[i]
            grouped = groups(page_objs)
            flattened = [o for g in grouped for o in g[1]]
            for ind, obj in enumerate(flattened):
                if obj['page_object'].cls == 'Figure Caption':
                    if after and (ind-1) > 0 and flattened[ind-1]['page_object'].cls == 'Figure':
                        figures.append([flattened[ind-1], flattened[ind]])
                    if not after and (ind+1) < len(flattened) and flattened[ind+1]['page_object'].cls == 'Figure':
                        figures.append([flattened[ind+1], flattened[ind]])

        final_objs = []
        for fig_fig_caption in figures:
            fig, fig_caption = fig_fig_caption
            aggregated_context = fig_caption['page_object'].content
            if aggregated_context.strip() == '':
                continue
            oc = ObjectContext(pdf_id=fig['page'].pdf_id,
                               cls='FigureContext',
                               header_id=fig["page_object"].id,
                               header_content=fig["page_object"].content,
                               content=aggregated_context)
            session.add(oc)
        session.commit()
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()
    return 'Ok'
    return final_objs



def aggregate_tables(objs):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    def filter_fn(obj):
        return obj['page_object'].cls in ['Table', 'Table Caption']
    try:
        fobjs = [o for o in objs if filter_fn(o)]
        pages = defaultdict(list)
        for obj in fobjs:
            pages[obj['page'].page_number].append(obj)

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
            # Decide if the caption comes after or before the table generally
            if flattened[0]['page_object'].cls == 'Table':
                after = True
            break

        for i in range(max_len):
            if i not in pages:
                continue
            page_objs = pages[i]
            grouped = groups(page_objs)
            flattened = [o for g in grouped for o in g[1]]
            for ind, obj in enumerate(flattened):
                if obj['page_object'].cls == 'Table':
                    if not after:
                        if (ind-1) > 0 and flattened[ind-1]['page_object'].cls == 'Table Caption':
                            tables.append([flattened[ind], flattened[ind-1]])
                        else:
                            tables.append([flattened[ind], None])

                    if after:
                        if (ind+1) < len(flattened) and flattened[ind+1]['page_object'].cls == 'Table Caption':
                            tables.append([flattened[ind], flattened[ind+1]])
                        else:
                            tables.append([flattened[ind], None])

        final_objs = []
        for tab_tab_caption in tables:
            tab, tab_caption = tab_tab_caption
            aggregated_context = f"{tab['page_object'].content}\n{tab_caption['page_object'].content}" if tab_caption is not None else tab['page_object'].content
            if aggregated_context.strip() == '':
                continue
            oc = ObjectContext(pdf_id=tab['page'].pdf_id,
                               cls='TableContext',
                               header_id=tab["page_object"].id,
                               header_content=tab["page_object"].content,
                               content=aggregated_context)
            session.add(oc)
        session.commit()
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()
    return 'Ok'


def aggregate_sections(objs):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@mysql-router:6446/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    try:
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
            oc = ObjectContext(pdf_id=obj['page'].pdf_id,
                               cls='Section',
                               header_id=header["page_object"].id,
                               header_content=header["page_object"].content,
                               content=aggregated_context)
            session.add(oc)
        session.commit()
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()

    return 'Ok'


def run():
    client = Client('scheduler:8786')
    process_dataset(os.environ['DATASET_ID'], client)

if __name__ == '__main__':
    run()


