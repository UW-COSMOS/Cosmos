from retrieval.elastic_retriever import ElasticRetriever
import click

@click.command()
@click.option('--delete/--no-delete', type=bool, help='')
@click.option('--load/--no-load', type=bool, help='')
@click.option('--search', default='', type=str, help='')
def run(delete, load, search):
    ret = ElasticRetriever()
    if load:
        ret.build_index('contracts.parquet')
    if delete:
        ret.delete(dataset_id='contracts')
    if search != '':
        ret.search(search)


if __name__ == '__main__':
    run()


