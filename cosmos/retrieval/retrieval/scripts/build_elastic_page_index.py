import click
from retrieval.elastic_page_retriever import ElasticPageRetriever
import os

@click.command()
@click.option('--documents-parquet', type=str, help='')
@click.option('--aws-host', type=str, help='', default='')
@click.option('--host', type=str, help='', default='localhost')
def run(documents_parquet, aws_host, host):
    if aws_host != '':
        auth = AWS4Auth(os.environ.get('AWS_ACCESS_KEY_ID'), os.environ.get('AWS_SECRET_ACCESS_KEY'), os.environ.get('AWS_DEFAULT_REGION'), 'es', session_token=os.environ.get('AWS_SESSION_TOKEN'))
        ret = ElasticPageRetriever(hosts=[{'host':aws_host, 'port':443}], awsauth=auth)
    else:
        ret = ElasticPageRetriever(hosts=[host])
    print('Connected to retriever, building indices')
    ret.build_index(documents_parquet)
    print('Done building index')

if __name__ == '__main__':
    run()
