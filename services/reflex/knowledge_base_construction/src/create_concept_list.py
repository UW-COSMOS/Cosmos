import pickle
import pandas as pd
import click

def open_pkl(file_path):
    results = pickle.load(open(file_path, 'rb'))
    return results

@click.command()
@click.argument('pickle_file')
@click.argument('output_file')
def extract(pickle_file, output_file):
    df = pd.read_pickle(pickle_file)

    topic_set = set()
    for ind, row in df.iterrows():
        subject = row['list_of_results']['sample']['sub_label']
        object = row['list_of_results']['sample']['obj_label']
        topic_set.add(subject)
        topic_set.add(object)
    with open(output_file, 'w') as wf:
        for t in topic_set:
            wf.write(f'{t}\n')

if __name__ == '__main__':
    extract()

