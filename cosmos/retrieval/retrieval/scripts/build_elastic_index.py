import click
from retrieval.elastic_retriever import ElasticRetriever
from requests_aws4auth import AWS4Auth
import os

@click.command()
@click.option('--sections-parquet', type=str, help='')
@click.option('--documents-parquet', type=str, help='')
@click.option('--aws-host', type=str, help='', default='')
def run(sections_parquet, documents_parquet, aws_host):
    if aws_host != '':
        auth = AWS4Auth(os.environ.get('AWS_ACCESS_KEY_ID'), os.environ.get('AWS_SECRET_ACCESS_KEY'), os.environ.get('AWS_DEFAULT_REGION'), 'es', session_token=os.environ.get('AWS_SESSION_TOKEN'))
        ret = ElasticRetriever(hosts=[{'host':aws_host, 'port':443}], awsauth=auth)
    else:
        ret = ElasticRetriever()
    print('Connected to retriever, building indices')
    ret.build_index(documents_parquet, sections_parquet)
    print('Done building index')

if __name__ == '__main__':
    run()