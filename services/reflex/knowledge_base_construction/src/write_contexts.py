import click
import json

@click.command()
@click.argument('doc-path')
def run(doc_path):
    with open(doc_path, 'r') as rf, open('contexts.jsonl', 'w') as wf:
        current_line_num = 0
        for line in rf:
            line = line.strip()
            if len(line) == 0:
                continue
            obj = {'id': f'doc{current_line_num}', 'text': line}
            objs = json.dumps(obj)
            result = f'{objs}\n'
            wf.write(result)
            current_line_num += 1


if __name__ == '__main__':
    run()

