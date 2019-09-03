from elasticsearch import Elasticsearch
from pymongo import MongoClient
from elasticsearch_dsl import Search, connections
import os
import logging
from bson.objectid import ObjectId
logging.basicConfig(format='%(levelname)s :: %(asctime)s :: %(message)s', level=logging.DEBUG)
import pickle
from table_extractions import extract_table_from_obj

def values_query(query):
    client = MongoClient(os.environ['DBCONNECT'])
    db = client.pdfs
    obj_type = 'TableContext'
    s = Search().query('match', content=query).query('match', cls=obj_type)[:40]
    response = s.execute()
    logging.info(str(response))
    result_list = []
    for result in response:
        logging.info(result.meta.id)

    content_set = set()
    for result in response:
        content = result.content
        logging.info(content)
        if content in content_set:
            continue
        content_set.add(content)
        id = result.meta.id
        obj_id = ObjectId(id)
        res = db.tableContexts.find_one({'_id': obj_id})
        pdf_name = res['table']['pdf_name']
        page_num = res['table']['page_num']
        coords = res['table']['bounding_box']
        logging.info(pdf_name)
        logging.info(page_num)
        logging.info(coords)
        logging.info('-----------')
        try:
            table_df, acc, whitespace= extract_table_from_obj(pdf_name, str(page_num), coords)
            if acc is None:
                continue
            if acc > 95 and whitespace < 15:
                result_list.append((table_df, res['table']['_id']))
        except ZeroDivisionError as e:
            logging.error(e)
            continue

    if len(result_list) == 0:
        return [], []

    
    vals = []
    oids = set()
    for result, oid in result_list:
        # Grab all cells containing the target query
        for col in result.columns:
            ser = result[col]
            msk = ser.str.contains(query, na=False)
            if True in msk:
                values = ser[ser.str.isdecimal()]
                val_list = values.tolist()
                val_list = [float(x) for x in val_list]
                vals.extend(val_list)
                if len(val_list) > 0:
                    oids.add(oid)
    oids = list(oids)

    return vals, oids
            



if __name__ == '__main__':
    connections.create_connection(hosts=['es01'], timeout=20)
    values_query('toc')

