"""
Aggregate text blobs into coherent sections
"""

import sys
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
from dask.distributed import Client, progress, fire_and_forget


import logging
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
MIN_SECTION_LEN = 30


def process_dataset(did, client):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
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
            n = session.query(ObjectContext).filter(ObjectContext.pdf_id == pdf_id).count()
            if n > 0:
                logging.info("Already aggregated this PDF!")
                continue
            res2 = session.query(Page.pdf_id, Page.page_number, PageObject.id, PageObject.content, PageObject.cls, PageObject.bounding_box).filter(Page.pdf_id == pdf_id)\
                    .filter(Page.id == PageObject.page_id)

            obj_list = []
            for pid, page_number, poid, po_content, po_cls, po_bb in res2:
                obj = {'pdf_id' : pid, 'page_number': page_number, 'po_id': poid, 'po_content': po_content, 'po_cls': po_cls, 'po_bb': po_bb}
                obj_list.append(obj)

            if len(obj_list) == 0:
                continue
            [r] = client.scatter([obj_list])
            r1 = client.submit(aggregate_sections, obj_list)
            fire_and_forget(r1)
            r2 = client.submit(aggregate_equations, r)
            fire_and_forget(r2)
            r3 = client.submit(aggregate_figures, r)
            fire_and_forget(r3)
            r4 = client.submit(aggregate_tables, r)
            fire_and_forget(r4)
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
        bb = p['po_bb']
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
        g[1].sort(key=lambda x: x['po_bb'][1])
    # Sort bins by x
    groups.sort(key=lambda x: x[0])
    return groups

def aggregate_equations(objs):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    try:
        def filter_fn(obj):
            return obj['po_cls'] in ['Equation', 'Body Text']

        fobjs = [o for o in objs if filter_fn(o)]
        pages = defaultdict(list)
        for obj in fobjs:
            pages[obj['page_number']].append(obj)

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
                    if obj['po_cls'] == 'Equation':
                        # Try to grab nearest body texts
                        assoc_text = []
                        # TODO: Associate previous page's context
                        if ind-1 >= 0 and group[1][ind-1]['po_cls'] == 'Body Text':
                            assoc_text.append(group[1][ind-1])
                        if ind+1 < len(group[1]):
                            if group[1][ind+1]['po_cls'] == 'Body Text':
                                assoc_text.append(group[1][ind+1])
                        elif gind+1 < len(grouped) and grouped[gind+1][1][0]['po_cls'] == 'Body Text': # Check the first item in the next group
                            assoc_text.append(grouped[gind+1][1][0])

                        equations.append([obj, assoc_text])

        final_objs = []
        for eq_contexts in equations:
            eq, contexts = eq_contexts
            update_ids = [eq['po_id']]
            aggregated_context = ''
            for context in contexts:
                update_ids.append(context['po_id'])
                aggregated_context += f'\n{context["po_content"]}\n'
            if aggregated_context.strip() == '' or len(aggregated_context.strip()) < MIN_SECTION_LEN:
                continue
            oc = ObjectContext(pdf_id=eq['pdf_id'],
                               cls='EquationContext',
                               header_id=eq["po_id"],
                               header_content=eq["po_content"],
                               content=aggregated_context)
            final_objects.append(oc)
            session.add(oc)
            session.commit()
            session.refresh(oc)
            for update_id in update_ids:
                res = session.query(PageObject).filter(PageObject.id == update_id).update({PageObject.context_id: oc.id}, synchronize_session = False)
            session.commit()
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()
    return 'Ok'

def aggregate_figures(objs):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    def filter_fn(obj):
        return obj['po_cls'] in ['Figure', 'Figure Caption']
    try:
        fobjs = [o for o in objs if filter_fn(o)]
        pages = defaultdict(list)
        for obj in fobjs:
            pages[obj['page_number']].append(obj)

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
            if flattened[0]['po_cls'] == 'Figure':
                after = True
            break

        for i in range(max_len):
            if i not in pages:
                continue
            page_objs = pages[i]
            grouped = groups(page_objs)
            flattened = [o for g in grouped for o in g[1]]
            for ind, obj in enumerate(flattened):
                if obj['po_cls'] == 'Figure Caption':
                    if after and (ind-1) > 0 and flattened[ind-1]['po_cls'] == 'Figure':
                        figures.append([flattened[ind-1], flattened[ind]])
                    if not after and (ind+1) < len(flattened) and flattened[ind+1]['po_cls'] == 'Figure':
                        figures.append([flattened[ind+1], flattened[ind]])

        final_objs = []
        for fig_fig_caption in figures:
            fig, fig_caption = fig_fig_caption
            update_ids = [fig['po_id'], fig_caption['po_id']]
            aggregated_context = fig_caption['po_content']
            if aggregated_context.strip() == '':
                continue
            oc = ObjectContext(pdf_id=fig['pdf_id'],
                               cls='FigureContext',
                               header_id=fig["po_id"],
                               header_content=fig["po_content"],
                               content=aggregated_context)
            session.add(oc)
            session.commit()
            session.refresh(oc)
            for update_id in update_ids:
                res = session.query(PageObject).filter(PageObject.id == update_id).update({PageObject.context_id: oc.id}, synchronize_session = False)
            session.commit()
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()
    return 'Ok'



def aggregate_tables(objs):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    def filter_fn(obj):
        return obj['po_cls'] in ['Table', 'Table Caption']
    try:
        fobjs = [o for o in objs if filter_fn(o)]
        pages = defaultdict(list)
        for obj in fobjs:
            pages[obj['page_number']].append(obj)

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
            if flattened[0]['po_cls'] == 'Table':
                after = True
            break

        for i in range(max_len):
            if i not in pages:
                continue
            page_objs = pages[i]
            grouped = groups(page_objs)
            flattened = [o for g in grouped for o in g[1]]
            for ind, obj in enumerate(flattened):
                if obj['po_cls'] == 'Table':
                    if not after:
                        if (ind-1) > 0 and flattened[ind-1]['po_cls'] == 'Table Caption':
                            tables.append([flattened[ind], flattened[ind-1]])
                        else:
                            tables.append([flattened[ind], None])

                    if after:
                        if (ind+1) < len(flattened) and flattened[ind+1]['po_cls'] == 'Table Caption':
                            tables.append([flattened[ind], flattened[ind+1]])
                        else:
                            tables.append([flattened[ind], None])

        final_objs = []
        for tab_tab_caption in tables:
            tab, tab_caption = tab_tab_caption
            update_ids = [tab['po_id']]
            if tab_caption is not None:
                update_ids.append(tab_caption['po_id'])
            update_ids = [fig['po_id'], fig_caption['po_id']]
            aggregated_context = f"{tab['po_content']}\n{tab_caption['po_content']}" if tab_caption is not None else tab['po_content']
            if aggregated_context.strip() == '':
                continue
            oc = ObjectContext(pdf_id=tab['pdf_id'],
                               cls='TableContext',
                               header_id=tab["po_id"],
                               header_content=tab["po_content"],
                               content=aggregated_context)
            session.add(oc)
            session.commit()
            session.refresh(oc)
            for update_id in update_ids:
                res = session.query(PageObject).filter(PageObject.id == update_id).update({PageObject.context_id: oc.id}, synchronize_session = False)
            session.commit()
    except Exception as e:
        logger.error(str(e), exc_info=True)
        session.rollback()
        raise e
    finally:
        session.close()
    return 'Ok'


def aggregate_sections(objs):
    engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@{os.environ["MYSQL_HOST"]}:{os.environ["MYSQL_PORT"]}/cosmos', pool_pre_ping=True)
    Session = sessionmaker()
    Session.configure(bind=engine)
    session = Session()
    try:
        def filter_fn(obj):
            return obj['po_cls'] in ['Body Text', 'Section Header']

        fobjs = [o for o in objs if filter_fn(o)]
        pages = defaultdict(list)
        for obj in fobjs:
            pages[obj['page_number']].append(obj)
        keys = pages.keys()
        if len(keys) == 0:
            # Probably should log something here
            return []
        max_len = max(keys)
        sections = []
        for i in range(max_len + 1):
            if i not in pages:
                continue
            page_objs = pages[i]
            grouped = groups(page_objs)
            current_section = None
            for group in grouped:
                for obj in group[1]:
                    if obj['po_cls'] == 'Section Header':
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
            update_ids = []
            aggregated_context = ''
            pdf_id = ''
            if header is not None:
                update_ids.append(header['po_id'])
                aggregated_context = f'{header["po_content"]}\n'
                pdf_id = header['pdf_id']
            else:
                pdf_id = objs[0]['pdf_id']
            for obj in objs:
                update_ids.append(obj['po_id'])
                aggregated_context += f'\n{obj["po_content"]}\n'
            if aggregated_context.strip() == '' or len(aggregated_context.strip()) < MIN_SECTION_LEN:
                continue
            objs = [{'_id': obj['po_id']} for obj in objs]
            if header is not None:
                oc = ObjectContext(pdf_id=obj['pdf_id'],
                                   cls='Section',
                                   header_id=header["po_id"],
                                   header_content=header["po_content"],
                                   content=aggregated_context)
            else:
                oc = ObjectContext(pdf_id=obj['pdf_id'],
                                   cls='Section',
                                   header_id=None,
                                   header_content=None,
                                   content=aggregated_context)
            session.add(oc)
            session.commit()
            session.refresh(oc)
            for update_id in update_ids:
                res = session.query(PageObject).filter(PageObject.id == update_id).update({PageObject.context_id: oc.id}, synchronize_session = False)
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


