#!/usr/bin/env python3
"""
"""

import os
from random import shuffle

if __name__ == '__main__':
    files = os.listdir('data/images')
    shuffle(files)
    # Split is 70/10/20
    filelen = len(files)
    train = int(.8 * filelen)
    val = int(.1 * filelen)
    train_filelist = files[:train]
    val_filelist = files[train:train+val]
    test_filelist = files[train+val:]
    print('There are {} train files, {} val files, and {} test files'.format(len(train_filelist),
                                                                             len(val_filelist),
                                                                             len(test_filelist)))
    with open('data/train.txt', 'w') as wf:
        for t in train_filelist:
            wf.write(t)
            wf.write('\n')

    with open('data/val.txt', 'w') as wf:
        for t in val_filelist:
            wf.write(t)
            wf.write('\n')

    with open('data/test.txt', 'w') as wf:
        for t in test_filelist:
            wf.write(t)
            wf.write('\n')

