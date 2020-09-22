import fasttext
import os

def make_vecs(df, n):
    content_series = df['content']
    result = content_series.str.cat(sep='\n')
    wpath = os.path.join('/mytmp', 'content.txt')
    with open(wpath, 'w') as wf:
        wf.write(result)
    model = fasttext.train_unsupervised(wpath, model='skipgram', wordNgrams=n)
    model.save_model(os.path.join('/output', 'vecs.bin'))
