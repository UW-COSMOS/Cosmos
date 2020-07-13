from retrieval.elastic_reranking_retriever import ElasticRerankingRetriever
import click

@click.command()
@click.option('--delete/--no-delete', type=bool, help='')
@click.option('--load/--no-load', type=bool, help='')
@click.option('--search', default='', type=str, help='')
def run(delete, load, search):
    ret = ElasticRerankingRetriever('tcp://localhost:8786')
    if load:
        ret.build_index('contracts.parquet')
    if delete:
        ret.delete(dataset_id='contracts')
    if search != '':
        results = ret.search(search)[:10]
        for ind, result in enumerate(results):
            c, s = result
            print(f'{ind}. Score: {s} Contexts: {c[:150] if len(c) >= 150 else c}')



if __name__ == '__main__':
    run()


