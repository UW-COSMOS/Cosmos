from extraction.qa_extractor import QAExtractor
import click


@click.command()
@click.option('--question', default='', type=str, help='')
@click.option('--context', default='', type=str, help='')
def run(question, context):
    model = QAExtractor('tcp://localhost:8786')
    result, score = model.extract(question, context)
    print(result)
    print(score)

if __name__ == '__main__':
    run()
