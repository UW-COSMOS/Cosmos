import torch
import click
import random
import json
from elasticsearch import Elasticsearch
from elasticsearch_dsl import Search, connections, Q
import pandas as pd
from fairseq import utils
from fairseq.data import (
    data_utils,
    Dictionary,
    encoders,
    IdDataset,
    ListDataset,
    NestedDictionaryDataset,
    NumSamplesDataset,
    NumelDataset,
    PadDataset,
    SortDataset,
)
from fairseq.tasks import FairseqTask, register_task


connections.create_connection(hosts=['es01'], timeout=20)

DATA_PATH = '/data/environ.csv'

NUM_NEGATIVE_EXAMPLES = 20
UPPER_WORD_DIST = 400
MAX_SUB_OBJ_DIST = 40
WINDOW_SIZE = 20
MIN_CONTEXT_SIZE = 40

def roberta_pretrained_exps():
    roberta = torch.hub.load('pytorch/fairseq', 'roberta.large')
    roberta.cuda()
    roberta.eval()
    f1 = 'large_roberta_output_object_filled.csv'
    f2 = 'large_roberta_output_subject_filled.csv'
    run_object_filled_exp(roberta, f1)
    run_subject_filled_exp(roberta, f2)


def run_object_filled_exp(model, filename):
    test_object_phrases = ['marine', 'fluvial', 'delta plain', 'lagoonal', 'marginal marine', 'outwash plain', 'weathering surface']
    with open(filename, 'w') as wf:
        for phrase in test_object_phrases:
            environ_completion = f'<mask> was deposited in a {phrase} environment.'
            results = model.fill_mask(environ_completion, topk=20)
            for result in results:
                wf.write(f'{result[0]},{result[1]}\n')


def run_subject_filled_exp(model):
    test_subject_phrases = ['Admiral', 'Abercrombie', 'Abel Gap', 'Abo', 'Bliss', 'Abrigo']
    with open(filename, 'w') as wf:
        for phrase in test_subject_phrases:
            environ_completion = f'{phrase} was deposited in a <mask> environment.'
            results = roberta.fill_mask(environ_completion, topk=20)
            for result in results:
                wf.write(f'{result[0]},{result[1]}\n')

class InputExample:
    def __init__(self, concept, subject, object, context, term, label):
        self.concept = concept
        self.subject = subject
        self.object = object
        self.context = context
        self.label = label
        self.matched_term = term


def get_noncontexts(phrase, nonphrase, context_len):
    s = Search()
    q = []
    q.append(Q('match', content=phrase))
    s.query = Q('bool', must=q)
    result = s.execute()
    noncontexts = []
    content_set = set()
    if context_len > 0:
        for h in result.hits:
            content = h['content']
            if content in content_set:
                continue
            content_set.add(content)
            if phrase in content and nonphrase not in content:
                content = content.replace('\n', ' ').strip()
                slist = content.split(' ')
                for i, s in enumerate(slist):
                    if phrase.split(' ')[0] in s:
                        ind = i
                low_ind = ind - WINDOW_SIZE if ind - WINDOW_SIZE > 0 else 0
                high_ind = ind + MAX_SUB_OBJ_DIST + WINDOW_SIZE if ind + MAX_SUB_OBJ_DIST + WINDOW_SIZE < len(slist)-1 else len(slist)-1
                sliced_content = ' '.join(slist[low_ind:high_ind])
                noncontexts.append(sliced_content)
                if len(noncontexts) == NUM_NEGATIVE_EXAMPLES * context_len:
                    break
    return noncontexts

def get_contexts(subject, object, both=True, obj_neg=False):
    s = Search()
    q = []
    q.append(Q('match', content=subject))
    q.append(Q('match', content=object))
    s.query = Q('bool', must=q)
    result = s.execute()
    contexts = []
    content_set = set()
    for h in result.hits:
        content = h['content']
        if content in content_set:
            continue
        content_set.add(content)
        if subject in content and object in content:
            content = content.replace('\n', ' ').strip()
            slist = content.split(' ')
            for i, s in enumerate(slist):
                if subject.split(' ')[0] in s:
                    sind = i
                if object.split(' ')[0] in s:
                    oind = i
            if abs(sind-oind) < MAX_SUB_OBJ_DIST:
                low_ind = sind if sind < oind else oind
                low_ind = low_ind - WINDOW_SIZE if low_ind - WINDOW_SIZE > 0 else 0
                high_ind = sind if sind > oind else oind
                high_ind = high_ind + WINDOW_SIZE if high_ind + WINDOW_SIZE < len(slist) - 1 else len(slist) - 1
                sliced_content = ' '.join(slist[low_ind:high_ind])
                contexts.append(sliced_content)


    # Now query for the subjects, but not the objects
    noncontexts_obj = get_noncontexts(object, subject, len(contexts))
    noncontexts_subj = get_noncontexts(subject, object, len(contexts))


    return contexts, noncontexts_obj, noncontexts_subj

def get_examples(both=True):
    df = pd.read_csv(DATA_PATH)
    df = df.drop_duplicates()
    df.columns = ['subject', 'object']
    def fn(e):
        if 'indet.' in e:
            return e.replace('indet.', '').strip()
        if 'inferred' in e:
            return e.replace('inferred', '').strip()
        return e
    df['object'] = df['object'].apply(fn)
    df['concept'] = 'was deposited in'
    examples = []
    for ind, row in df.iterrows():
        contexts, noncontexts_obj, noncontexts_subj = get_contexts(row['subject'], row['object'])
        concept = row['concept']
        for context in contexts:
            ex = InputExample(concept, row['subject'], row['object'], context, 'both', '1')
            examples.append(ex)
        if both:
            for noncontext in noncontexts_obj:
                ex = InputExample(concept, row['subject'], row['object'], noncontext, 'object', '0')
                examples.append(ex)
            for noncontext in noncontexts_subj:
                ex = InputExample(concept, row['subject'], row['object'], noncontext, 'subject', '0')
                examples.append(ex)
        elif obj_neg:
            for noncontext in noncontexts_obj:
                ex = InputExample(concept, row['subject'], row['object'], noncontext, 'object', '0')
                examples.append(ex)
        else:
            for noncontext in noncontexts_subj:
                ex = InputExample(concept, row['subject'], row['object'], noncontext, 'subject', '0')
                examples.append(ex)


    splits = {'train': 0.9, 'valid': 0.05, 'test': 0.05}
    marker = 0
    random.shuffle(examples)
    objs = [ {
               'concept': ex.concept,
               'subject': ex.subject,
               'object': ex.object,
               'context': ex.context,
               'matched_term': ex.matched_term,
               'label': ex.label,
             } for ex in examples ]
    print(len(examples))
    for spl in splits:
        end = int(len(examples) * splits[spl]) + marker
        exs = examples[marker:end]
        marker = end

        with open(f'{spl}.jsonl', 'w') as wf:
            for ex in exs:
                obj = {
                        'concept': ex.concept,
                        'subject': ex.subject,
                        'object': ex.object,
                        'context': ex.context,
                        'matched_term': ex.matched_term,
                        'label': ex.label,
                      }
                jobj = json.dumps(obj)
                wf.write(f'{jobj}\n')

    return objs

def print_full(x):
    pd.set_option('display.max_rows', None)
    pd.set_option('display.max_columns', None)
    pd.set_option('display.width', 2000)
    pd.set_option('display.float_format', '{:20,.2f}'.format)
    pd.set_option('display.max_colwidth', -1)
    print(x)
    pd.reset_option('display.max_rows')
    pd.reset_option('display.max_columns')
    pd.reset_option('display.width')
    pd.reset_option('display.float_format')
    pd.reset_option('display.max_colwidth')

@click.command()
@click.option('--examples/--noexamples')
def run(examples):
    #roberta_pretrained_exps()
    if examples:
        examples = get_examples()
        df = pd.DataFrame(examples)
        print(df)
        print(f"Number of negative contexts: {len(df[df['label'] == '0'])}")
        print(f"Number of positive contexts: {len(df[df['label'] == '1'])}")
        print(f"Number of examples: {len(df)}")
        print(f"Object group by: {df.groupby('object').agg(['count'])}")
        print_full(f"Subject group by: {df.groupby('subject').agg(['count'])}")

    #get_context(None, 'marine')


if __name__ == '__main__':
    run()



