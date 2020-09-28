import click
from retrieval.elastic_retriever import ElasticRetriever
from requests_aws4auth import AWS4Auth
import os

@click.command()
@click.option('--dataset-id', type=str, help='')
@click.option('--aws-host', type=str, help='', default='')
@click.option('--host', type=str, help='', default='localhost')
def run(dataset_id, aws_host, host):
    if aws_host != '':
        auth = AWS4Auth(os.environ.get('AWS_ACCESS_KEY_ID'), os.environ.get('AWS_SECRET_ACCESS_KEY'), os.environ.get('AWS_DEFAULT_REGION'), 'es', session_token=os.environ.get('AWS_SESSION_TOKEN'))
        ret = ElasticRetriever(hosts=[{'host':aws_host, 'port':443}], awsauth=auth)
    else:
        ret = ElasticRetriever(hosts=[host])
    print('Connected to retriever, building indices')
    ret.delete(dataset_id)
    print('Done deleting index')

if __name__ == '__main__':
    run()