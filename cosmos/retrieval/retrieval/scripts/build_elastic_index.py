import click
from retrieval.elastic_retriever import ElasticRetriever
import os

@click.command()
@click.option('--sections-parquet', type=str, help='', default='')
@click.option('--documents-parquet', type=str, help='', default='')
@click.option('--tables-parquet', type=str, help='', default='')
@click.option('--figures-parquet', type=str, help='', default='')
@click.option('--equations-parquet', type=str, help='', default='')
@click.option('--aws-host', type=str, help='', default='')
@click.option('--host', type=str, help='', default='localhost')
def run(sections_parquet, documents_parquet, tables_parquet, figures_parquet, equations_parquet, aws_host, host):
    if aws_host != '':
        auth = AWS4Auth(os.environ.get('AWS_ACCESS_KEY_ID'), os.environ.get('AWS_SECRET_ACCESS_KEY'), os.environ.get('AWS_DEFAULT_REGION'), 'es', session_token=os.environ.get('AWS_SESSION_TOKEN'))
        ret = ElasticRetriever(hosts=[{'host':aws_host, 'port':443}], awsauth=auth)
    else:
        ret = ElasticRetriever(hosts=[host])
    print('Connected to retriever, building indices')
    ret.build_index(documents_parquet, sections_parquet, tables_parquet, figures_parquet, equations_parquet)
    print('Done building index')

if __name__ == '__main__':
    run()
