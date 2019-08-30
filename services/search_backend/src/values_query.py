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
    s = Search().query('match', content=query).query('match', cls=obj_type)[:20]
    response = s.execute()
    logging.info(str(response))
    result_list = []
    for result in response:
        id = result.meta.id
        obj_id = ObjectId(id)
        res = db.tableContexts.find_one({'_id': obj_id})
        pdf_name = res['table']['pdf_name']
        page_num = res['table']['page_num']
        coords = res['table']['bounding_box']
        try:
            table_df, acc = extract_table_from_obj(pdf_name, str(page_num), coords)
            if acc is None:
                continue
            if acc > 90:
                table_df = pickle.loads(table_df)
                result_list.append(table_df)
        except ZeroDivisionError as e:
            logging.error(e)
            continue

    for r in result_list:
        print(r)




if __name__ == '__main__':
    connections.create_connection(hosts=['es01'], timeout=20)
    values_query('toc')

