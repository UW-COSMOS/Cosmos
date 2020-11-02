from dask.distributed import get_worker, Client, progress
import pandas as pd
import logging
import click
import glob
import os
logger = logging.getLogger(__name__)
logger.setLevel(logging.ERROR)

def link(content, score_threshold=0.8):
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'LinkingPlugin' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise Exception('No linking plugin registered')

        linking_result = dp.nlp(content)
        ent_list = set()
        nonlinked_list = set() # We'll only add one copy of the entity mention per paragraph.
        for ent in linking_result.ents:
            linked = False
            for ent_id, score in ent._.kb_ents:
                if score > score_threshold:
                    linked = True
                    ent_list.add(ent_id)
                break
            if not linked:
                nonlinked_list.add(ent.text)

        ent_list = list(ent_list)
        nonlinked_list = list(nonlinked_list)
        return ent_list, nonlinked_list

    except Exception as e:
        logger.error(str(e), exc_info=True)
        return (None, None)

@click.command()
@click.option('input_path', type=str, help='Input directory full of parquets to link using an existing dask cluster')
@click.option('output_path', type=str, help='output directory')
@click.option('cluster', type=str, help='Scheduler address of dask cluster')
def run_link(input_path, output_path, cluster):
    logger.info("Setting up client")
    client = Client(cluster, serializers=['msgpack', 'dask'], deserializers=['msgpack', 'dask', 'pickle'])
    logger.info(client)
    # Assumption: The input parquet fits into memory. Will need to switch to dask distributed otherwise
    for pq in glob.glob(os.path.join(input_path, '*.parquet')):
        df = pd.read_parquet(pq)
        contents = df['content'].tolist()
        results = [client.submit(link, c, resources={'linking': 1}) for c in contents]
        progress(results)
        results = [r.result() for r in results]
        ent_lists, nonlinked_lists = zip(*results)
        df['ents_linked'] = ent_lists
        df['ents_unlinked'] = nonlinked_lists
        basename = os.path.basename(pq)
        df.to_parquet(os.path.join(output_path, basename))


if __name__ == '__main__':
    run_link()
