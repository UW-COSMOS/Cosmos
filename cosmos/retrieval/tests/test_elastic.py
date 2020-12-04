from retrieval.elastic_retriever import ElasticRetriever
import click

@click.command()
@click.option('--delete/--no-delete', type=bool, help='')
@click.option('--load/--no-load', type=bool, help='')
@click.option('--search', default='', type=str, help='')
@click.option('--entity-search/--no-entity-search', default=True, type=bool, help='')
@click.option('--cls', default='Table', type=str, help='')
@click.option('--host', default='localhost', type=str, help='')
def run(delete, load, search, entity_search, cls, host):
    ret = ElasticRetriever(hosts=[host])
    if load:
        ret.build_index('contracts.parquet')
    if delete:
        ret.delete(dataset_id='contracts')
    if search != '':
        result = ret.search(search, entity_search=entity_search, cls=cls, ndocs=1)
        print(result)


if __name__ == '__main__':
    run()


