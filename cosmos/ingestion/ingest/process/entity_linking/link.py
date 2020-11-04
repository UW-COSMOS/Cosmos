from dask.distributed import get_worker, Client, progress
import pandas as pd
import logging
import click
import glob
import os
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


def link(content, score_threshold=0.8):
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'Linking' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise Exception('No linking plugin registered')

        linking_result = dp.nlp(content)
        ent_set = set()
        nonlinked_list = set() # We'll only add one copy of the entity mention per paragraph.
        for ent in linking_result.ents:
            linked = False
            for ent_id, score in ent._.kb_ents:
                if score > score_threshold:
                    linked = True
                    if ent_id in ent_set:
                        continue
                    ent_set.add(ent_id)
                break
            if not linked:
                nonlinked_list.add(ent.text)

        ent_set = list(ent_set)
        nonlinked_list = list(nonlinked_list)
        return nonlinked_list, ent_set

    except Exception as e:
        logger.error(str(e), exc_info=True)
        return (None, None)


def construct_linked_kb(eids):
    try:
        worker = get_worker()
        dp = None
        for plg in worker.plugins:
            if 'Linking' in plg:
                dp = worker.plugins[plg]
                break
        if dp is None:
            raise Exception('No linking plugin registered')
        if eids is None:
            return None
        results = []
        for eid in eids:
            entity = dp.linker.kb.cui_to_entity[eid]
            result = {
                'id': entity.concept_id,
                'name': entity.canonical_name,
                'aliases': tuple(entity.aliases),
                'types': tuple(entity.types),
                'description': entity.definition
            }
            results.append(result)
        result_df = pd.DataFrame(results)
        return result_df
    except Exception as e:
        logger.error(str(e), exc_info=True)
        return None


@click.command()
@click.option('--input-path', type=str, help='Input directory full of parquets to link using an existing dask cluster')
@click.option('--output-path', type=str, help='output directory')
@click.option('--cluster', type=str, help='Scheduler address of dask cluster')
@click.option('--dataset-id', type=str, help='dataset id to put in output entities file')
def run_link(input_path, output_path, cluster, dataset_id):
    logger.info("Setting up client")
    client = Client(cluster, serializers=['msgpack', 'dask'], deserializers=['msgpack', 'dask', 'pickle'])
    logger.info(client)
    full_ent_set = []
    # Assumption: The input parquet fits into memory. Will need to switch to dask distributed otherwise
    for pq in glob.glob(os.path.join(input_path, '*.parquet')):
        df = pd.read_parquet(pq)
        contents = df['content'].tolist()
        results = [client.submit(link, c, resources={'linking': 1}) for c in contents]
        progress(results)
        results = [r.result() for r in results]
        nonlinked_lists, ent_set = zip(*results)
        nonlinked_lists = list(nonlinked_lists)
        ent_set = list(ent_set)
        ent_set = [list(e) if e is not None or len(e) > 0 else None for e in ent_set]
        full_ent_set.extend(ent_set)
        nonlinked_lists = [list(e) for e in nonlinked_lists]
        df['ents_linked'] = ent_set
        df['ents_unlinked'] = nonlinked_lists
        basename = os.path.basename(pq)
        df.to_parquet(os.path.join(output_path, basename))

    logger.info('Starting entity info extraction')
    dfs = [client.submit(construct_linked_kb, e, resources={'linking': 1}) for e in full_ent_set]
    progress(dfs)
    dfs = [d.result() for d in dfs]
    dfs = [d for d in dfs if d is not None]
    dfs = pd.concat(dfs)
    dfs.drop_duplicates(inplace=True)
    dfs['aliases'] = dfs.apply(lambda row: list(row['aliases']), axis=1)
    dfs['types'] = dfs.apply(lambda row: list(row['types']), axis=1)
    dfs['dataset_id'] = dataset_id
    dfs.to_parquet(os.path.join(output_path, f'{dataset_id}_entities.parquet'))


if __name__ == '__main__':
    run_link()
